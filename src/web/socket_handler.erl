-module(socket_handler).

%% @doc WebSocket handler for lobby matchmaking and in-room messages.

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-record(state, {player_info = #{}, player_id, room_id}).

-type ws_state() :: #state{}.
-type payload() :: map().
-type ws_frame() :: {text, iodata()}.

%% ------------------------------------------------------------------
%% Cowboy callbacks.
%% ------------------------------------------------------------------

-spec init(cowboy_req:req(), term()) -> {cowboy_websocket, cowboy_req:req(), ws_state()}.
%% @doc Upgrade an HTTP request to a WebSocket and capture player or reconnect query data.
%% Sample usage: configured in routes as `{"/ws", socket_handler, []}`.
init(Req, _State) ->
    Params = cowboy_req:parse_qs(Req),
    Query = maps:from_list(Params),
    PlayerParam = maps:get(<<"player">>, Query, <<"player">>),
    PlayerIdParam = maps:get(<<"player_id">>, Query, undefined),
    RoomIdParam = maps:get(<<"room_id">>, Query, undefined),
    RulesetParam = query_value(<<"ruleset">>, Query, <<"battleship">>),
    PlayerInfo = Query#{name => PlayerParam, ruleset => RulesetParam},
    State =
        case {RoomIdParam, PlayerIdParam} of
            {RoomId, PlayerId} when is_binary(RoomId), is_binary(PlayerId) ->
                #state{
                    player_info = PlayerInfo,
                    player_id = PlayerId,
                    room_id = RoomId
                };
            _ ->
                #state{player_info = PlayerInfo}
        end,
    {cowboy_websocket, Req, State}.

-spec query_value(binary(), map(), term()) -> term().
query_value(Key, Query, Default) ->
    maps:get(Key, Query, Default).

-spec websocket_init(ws_state()) -> {[ws_frame()], ws_state()}.
%% @doc Join matchmaking or reconnect once the WebSocket process starts.
%% Sample usage: called by Cowboy after WebSocket upgrade.
websocket_init(State = #state{room_id = RoomId, player_id = PlayerId}) when
    is_binary(RoomId), is_binary(PlayerId)
->
    case room:reconnect(RoomId, PlayerId, self()) of
        {ok, #{view := View, opponent_id := OpponentId}} ->
            Payload = room_events:rejoined(RoomId, PlayerId, OpponentId, View),
            {[{text, json:encode(Payload)}], State};
        {error, Reason} ->
            Payload = #{type => <<"error">>, reason => reconnect_error(Reason)},
            {[{text, json:encode(Payload)}], State}
    end;
websocket_init(State = #state{player_info = PlayerInfo}) ->
    case lobby:join(self(), PlayerInfo) of
        {waiting, PlayerId} ->
            Payload = #{type => <<"lobby_waiting">>, player_id => PlayerId},
            {[{text, json:encode(Payload)}], State#state{player_id = PlayerId}};
        {matched, PlayerId, OpponentId, RoomId} ->
            Payload = match_payload(PlayerId, OpponentId, RoomId),
            {[{text, json:encode(Payload)}], State#state{
                player_id = PlayerId,
                room_id = RoomId
            }};
        {error, Reason} ->
            Payload = #{type => <<"error">>, reason => join_error(Reason)},
            {[{text, json:encode(Payload)}], State}
    end.

-spec websocket_handle(term(), ws_state()) -> {[ws_frame()], ws_state()}.
%% @doc Handle incoming WebSocket frames from the browser.
%% Sample usage: client sends `#{type => <<"move">>, row => 0, column => 0}` as JSON.
websocket_handle({text, Msg}, State) ->
    handle_message(Msg, State);
websocket_handle(_Data, State) ->
    {[], State}.

-spec websocket_info(term(), ws_state()) -> {[ws_frame()], ws_state()}.
%% @doc Send room payloads produced by lobby or room processes to the WebSocket client.
%% Sample usage: `Pid ! {socket_send, room_events:update(RoomId, View)}`.
websocket_info({socket_send, Payload}, State) ->
    NewState = update_state_from_payload(Payload, State),
    {[{text, json:encode(Payload)}], NewState};
websocket_info(_Info, State) ->
    {[], State}.

-spec terminate(term(), cowboy_req:req(), ws_state()) -> ok.
%% @doc Leave lobby and room tracking when the WebSocket terminates.
%% Sample usage: called by Cowboy when a socket closes.
terminate(_Reason, _Req, State) ->
    lobby:leave(self()),
    case {State#state.room_id, State#state.player_id} of
        {undefined, _} -> ok;
        {_, undefined} -> ok;
        {RoomId, PlayerId} -> room:leave(RoomId, PlayerId, self())
    end,
    ok.

%% ------------------------------------------------------------------
%% Private helpers.
%% ------------------------------------------------------------------

-spec handle_message(binary(), ws_state()) -> {[ws_frame()], ws_state()}.
handle_message(Msg, State = #state{room_id = RoomId, player_id = PlayerId}) ->
    try json:decode(Msg) of
        #{<<"type">> := _Type} = Payload ->
            submit_payload(RoomId, PlayerId, Payload, State);
        _ ->
            Error = #{type => <<"error">>, reason => <<"unknown_message">>},
            {[{text, json:encode(Error)}], State}
    catch
        _:_ ->
            Error = #{type => <<"error">>, reason => <<"invalid_payload">>},
            {[{text, json:encode(Error)}], State}
    end.

-spec match_payload(binary(), binary(), binary()) -> payload().
match_payload(PlayerId, OpponentId, RoomId) ->
    room_events:match_found(PlayerId, OpponentId, RoomId).

-spec reconnect_error(room_not_found | unknown_player) -> binary().
reconnect_error(room_not_found) ->
    <<"room_not_found">>;
reconnect_error(unknown_player) ->
    <<"unknown_player">>.

-spec join_error(unsupported_ruleset) -> binary().
join_error(unsupported_ruleset) ->
    <<"unsupported_ruleset">>.

-spec submit_payload(binary() | undefined, binary() | undefined, map(), ws_state()) ->
    {[ws_frame()], ws_state()}.
submit_payload(undefined, _PlayerId, _Payload, State) ->
    Error = #{type => <<"error">>, reason => <<"no_room">>},
    {[{text, json:encode(Error)}], State};
submit_payload(_RoomId, undefined, _Payload, State) ->
    Error = #{type => <<"error">>, reason => <<"no_player">>},
    {[{text, json:encode(Error)}], State};
submit_payload(RoomId, PlayerId, Payload, State) when is_binary(RoomId), is_binary(PlayerId) ->
    _ = room:submit(RoomId, PlayerId, action_payload(Payload)),
    {[], State}.

-spec action_payload(map()) -> map().
action_payload(Payload) ->
    with_atom_key(<<"column">>, column, with_atom_key(<<"row">>, row, Payload)).

-spec with_atom_key(term(), atom(), map()) -> map().
with_atom_key(FromKey, ToKey, Payload) ->
    case maps:find(FromKey, Payload) of
        {ok, Value} -> Payload#{ToKey => Value};
        error -> Payload
    end.

-spec update_state_from_payload(payload(), ws_state()) -> ws_state().
update_state_from_payload(
    #{type := <<"match_found">>, room_id := RoomId, player_id := PlayerId}, State
) ->
    State#state{room_id = RoomId, player_id = PlayerId};
update_state_from_payload(_Payload, State) ->
    State.
