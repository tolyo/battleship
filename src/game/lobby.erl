-module(lobby).
-behaviour(gen_server).

%% @doc Matchmaking lobby that pairs players into rooms.

-export([start_link/0, join/2, leave/1, list_rooms/0, room_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type room_id() :: binary().
-type player_id_bin() :: binary().
-type player_info() :: map().
-type join_reply() ::
    {waiting, player_id_bin()}
    | {matched, player_id_bin(), player_id_bin(), room_id()}
    | {error, unsupported_ruleset}.
-type room_entry() :: #{pid := pid(), ref := reference()}.

-record(lobby_player, {
    pid :: pid(),
    ref :: reference(),
    player_id :: player_id_bin(),
    name :: binary(),
    rules :: module(),
    info :: map()
}).
-record(state, {
    waiting = [] :: [#lobby_player{}],
    rooms = #{} :: #{room_id() => room_entry()},
    counter = 0 :: non_neg_integer()
}).

%% ------------------------------------------------------------------
%% Public API.
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
%% @doc Start the lobby process registered as `lobby`.
%% Sample usage: `{ok, Pid} = lobby:start_link().`
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec join(pid(), player_info()) -> join_reply().
%% @doc Join matchmaking with player setup data and either wait or receive a match.
%% Sample usage: `lobby:join(self(), #{name => <<"Ada">>, ruleset => <<"battleship">>}).`
join(Pid, PlayerInfo) when is_pid(Pid) ->
    case gen_server:call(?MODULE, {join, Pid, PlayerInfo}) of
        {waiting, PlayerId} = Reply when is_binary(PlayerId) ->
            Reply;
        {matched, PlayerId, OpponentId, RoomId} = Reply when
            is_binary(PlayerId), is_binary(OpponentId), is_binary(RoomId)
        ->
            Reply;
        {error, unsupported_ruleset} = Reply ->
            Reply
    end.

-spec leave(pid()) -> ok.
%% @doc Remove a waiting player from matchmaking.
%% Sample usage: `lobby:leave(self()).`
leave(Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {leave, Pid}).

-spec list_rooms() -> [room_id()].
%% @doc Return the ids of rooms currently tracked by the lobby.
%% Sample usage: `RoomIds = lobby:list_rooms().`
list_rooms() ->
    ensure_room_ids(gen_server:call(?MODULE, list_rooms)).

-spec room_pid(room_id()) -> {ok, pid()} | {error, not_found}.
%% @doc Look up a tracked room process by room id.
%% Sample usage: `lobby:room_pid(<<"room-1">>).`
room_pid(RoomId) ->
    case gen_server:call(?MODULE, {room_pid, RoomId}) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        {error, not_found} ->
            {error, not_found}
    end.

%% ------------------------------------------------------------------
%% gen_server callbacks.
%% ------------------------------------------------------------------

-spec init([]) -> {ok, #state{waiting :: [], rooms :: #{}, counter :: 0}}.
%% @doc Initialize an empty lobby state.
%% Sample usage: called by OTP when `lobby:start_link/0` starts.
init([]) ->
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply,
        [room_id()]
        | {error, not_found | unknown_request | unsupported_ruleset}
        | {ok, pid()}
        | join_reply(),
        #state{}}.
%% @doc Handle lobby lookup and matchmaking requests.
%% Sample usage: called by OTP for `gen_server:call(lobby, Request)`.
handle_call(list_rooms, _From, State = #state{}) ->
    RoomIds = maps:keys(State#state.rooms),
    {reply, RoomIds, State};
handle_call({room_pid, RoomId}, _From, State = #state{}) ->
    case maps:get(RoomId, State#state.rooms, undefined) of
        #{pid := Pid} -> {reply, {ok, Pid}, State};
        _ -> {reply, {error, not_found}, State}
    end;
handle_call({join, Pid, PlayerInfo}, _From, State = #state{}) ->
    case build_player(Pid, PlayerInfo) of
        {ok, Player} ->
            match_player(Player, State);
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(_Msg, _From, State = #state{}) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
%% @doc Handle asynchronous lobby messages such as leaving matchmaking.
%% Sample usage: called by OTP for `gen_server:cast(lobby, {leave, Pid})`.
handle_cast({leave, Pid}, State = #state{}) ->
    NewWaiting = [Player || Player <- State#state.waiting, Player#lobby_player.pid =/= Pid],
    {noreply, State#state{waiting = NewWaiting}};
handle_cast(_Msg, State = #state{}) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
%% @doc Handle monitored player and room process exits.
%% Sample usage: called by OTP for process `DOWN` messages.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State = #state{}) ->
    NewWaiting = [Player || Player <- State#state.waiting, Player#lobby_player.ref =/= Ref],
    Rooms = remove_room_by_ref(Ref, State#state.rooms),
    {noreply, State#state{waiting = NewWaiting, rooms = Rooms}};
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
%% @doc Lobby termination hook.
%% Sample usage: called by OTP during supervisor shutdown.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
%% @doc Preserve lobby state during hot-code upgrades.
%% Sample usage: called by OTP release handling.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Private helpers.
%% ------------------------------------------------------------------

-spec build_player(pid(), player_info()) ->
    {ok,
        #lobby_player{
            pid :: pid(),
            ref :: reference(),
            player_id :: binary(),
            name :: binary(),
            rules :: module(),
            info :: map()
        }}
    | {error, unsupported_ruleset}.
build_player(Pid, PlayerInfo) ->
    case rules_registry:module_for(PlayerInfo) of
        {ok, Rules} ->
            PlayerId = maps:get(player_id, PlayerInfo, make_player_id()),
            Name = maps:get(name, PlayerInfo, <<"player">>),
            Info = Rules:prepare_player(PlayerInfo#{pid => Pid, id => PlayerId, name => Name}),
            Ref = erlang:monitor(process, Pid),
            {ok, #lobby_player{
                pid = Pid,
                ref = Ref,
                player_id = PlayerId,
                name = Name,
                rules = Rules,
                info = Info
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec match_player(#lobby_player{}, #state{}) -> {reply, join_reply(), #state{}}.
match_player(Player, State) ->
    case take_waiting_player(Player#lobby_player.rules, State#state.waiting) of
        none ->
            NewState = State#state{waiting = [Player | State#state.waiting]},
            {reply, {waiting, Player#lobby_player.player_id}, NewState};
        {Waiting, Rest} ->
            erlang:demonitor(Waiting#lobby_player.ref, [flush]),
            erlang:demonitor(Player#lobby_player.ref, [flush]),
            RoomId = make_room_id(State#state.counter + 1),
            Player1 = lobby_player_to_room_player(Waiting),
            Player2 = lobby_player_to_room_player(Player),
            {ok, RoomPid} = room_sup:start_room(
                RoomId,
                Player#lobby_player.rules,
                [Player1, Player2],
                #{}
            ),
            RoomRef = erlang:monitor(process, RoomPid),
            Rooms = (State#state.rooms)#{RoomId => #{pid => RoomPid, ref => RoomRef}},
            notify_player_match(Waiting, Player#lobby_player.player_id, RoomId),
            {reply,
                {matched, Player#lobby_player.player_id, Waiting#lobby_player.player_id, RoomId},
                State#state{
                    waiting = Rest,
                    rooms = Rooms,
                    counter = State#state.counter + 1
                }}
    end.

-spec take_waiting_player(module(), [#lobby_player{}]) ->
    none | {#lobby_player{}, [#lobby_player{}]}.
take_waiting_player(Rules, Waiting) ->
    take_waiting_player(Rules, Waiting, []).

take_waiting_player(_Rules, [], _Skipped) ->
    none;
take_waiting_player(Rules, [Player = #lobby_player{rules = Rules} | Rest], Skipped) ->
    {Player, lists:reverse(Skipped) ++ Rest};
take_waiting_player(Rules, [Player | Rest], Skipped) ->
    take_waiting_player(Rules, Rest, [Player | Skipped]).

-spec ensure_room_ids([room_id()]) -> [room_id()].
ensure_room_ids([]) ->
    [];
ensure_room_ids([RoomId | Rest]) when is_binary(RoomId) ->
    [RoomId | ensure_room_ids(Rest)].

-spec make_player_id() -> player_id_bin().
make_player_id() ->
    integer_to_binary(erlang:unique_integer([monotonic, positive])).

-spec make_room_id(pos_integer()) -> binary().
make_room_id(Counter) ->
    <<<<"room-">>/binary, (integer_to_binary(Counter))/binary>>.

-spec lobby_player_to_room_player(#lobby_player{}) -> map().
lobby_player_to_room_player(#lobby_player{
    pid = Pid, player_id = PlayerId, name = Name, info = Info
}) ->
    Info#{pid => Pid, id => PlayerId, name => Name}.

-spec notify_player_match(#lobby_player{}, player_id_bin(), room_id()) -> ok.
notify_player_match(
    #lobby_player{pid = Pid, player_id = PlayerId, name = Name}, OpponentId, RoomId
) ->
    Pid ! {socket_send, room_events:match_found(PlayerId, OpponentId, RoomId, Name)},
    ok.

-spec remove_room_by_ref(reference(), #{room_id() => room_entry()}) -> #{room_id() => room_entry()}.
remove_room_by_ref(Ref, Rooms) when is_reference(Ref) ->
    maps:filter(
        fun(_RoomId, RoomInfo) ->
            maps:get(ref, RoomInfo) =/= Ref
        end,
        Rooms
    ).
