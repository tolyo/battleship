-module(room).
-behaviour(gen_statem).

%% @doc Generic room process driven by a rules module.

-export([
    start_link/4,
    submit/3,
    leave/2,
    leave/3,
    reconnect/3,
    state/1
]).
-export([callback_mode/0, init/1, setup/3, active/3, finished/3, terminate/3, code_change/4]).

-type room_id() :: binary().
-type player_id_bin() :: binary().
-type rules_module() :: module().
-type player_info() :: #{
    pid := pid(),
    id := player_id_bin(),
    name := binary()
}.
-type player_entry() :: #{
    pid := pid() | undefined,
    id := player_id_bin(),
    name := binary(),
    ref := reference() | undefined
}.
-type players_map() :: #{player_id_bin() => player_entry()}.
-type action_result() :: ok | {error, binary() | room_not_found}.
%% @doc Protocol view after the room attaches generic metadata such as allowed actions.
-type protocol_view() :: rules:view().

-record(data, {
    room_id :: room_id(),
    rules :: rules_module(),
    rules_state :: rules:state(),
    players = #{} :: players_map()
}).

%% ------------------------------------------------------------------
%% Public API.
%% ------------------------------------------------------------------

-spec start_link(room_id(), rules_module(), [player_info()], map()) ->
    {ok, pid()} | ignore | {error, term()}.
%% @doc Start a room state machine with a rules module and prepared players.
%% Sample usage: `room:start_link(<<"room-1">>, battleship_rules, [P1, P2], #{}).`
start_link(RoomId, Rules, Players, Options) when is_binary(RoomId), is_atom(Rules) ->
    gen_statem:start_link(?MODULE, [RoomId, Rules, [ensure_player_info(P) || P <- Players], Options], []).

-spec submit(room_id(), player_id_bin(), map()) -> action_result().
%% @doc Submit one generic player action to a lobby-registered room.
%% Sample usage: `room:submit(<<"room-1">>, <<"p1">>, #{type => <<"move">>, row => 0, column => 0}).`
submit(RoomId, PlayerId, Action) when is_binary(RoomId), is_binary(PlayerId), is_map(Action) ->
    case lobby:room_pid(RoomId) of
        {ok, Pid} -> ensure_action_result(gen_statem:call(Pid, {action, PlayerId, Action}));
        {error, _} -> {error, room_not_found}
    end.

-spec leave(room_id(), player_id_bin()) -> ok.
%% @doc Mark a player as disconnected from a room.
%% Sample usage: `room:leave(<<"room-1">>, <<"p1">>).`
leave(RoomId, PlayerId) when is_binary(RoomId), is_binary(PlayerId) ->
    leave(RoomId, PlayerId, undefined).

-spec leave(room_id(), player_id_bin(), pid() | undefined) -> ok.
%% @doc Mark a player pid as disconnected from a room, ignoring stale pids.
%% Sample usage: `room:leave(<<"room-1">>, <<"p1">>, self()).`
leave(RoomId, PlayerId, Pid) when is_binary(RoomId), is_binary(PlayerId) ->
    case lobby:room_pid(RoomId) of
        {ok, RoomPid} -> gen_statem:cast(RoomPid, {leave, PlayerId, Pid});
        {error, _} -> ok
    end.

-spec reconnect(room_id(), player_id_bin(), pid()) ->
    {ok, #{view := map(), opponent_id := player_id_bin()}}
    | {error, room_not_found | unknown_player}.
%% @doc Attach a new pid to an existing player and return the current room view.
%% Sample usage: `room:reconnect(<<"room-1">>, <<"p1">>, self()).`
reconnect(RoomId, PlayerId, Pid) when is_binary(RoomId), is_binary(PlayerId), is_pid(Pid) ->
    case lobby:room_pid(RoomId) of
        {ok, RoomPid} ->
            ensure_reconnect_result(gen_statem:call(RoomPid, {reconnect, PlayerId, Pid}));
        {error, _} ->
            {error, room_not_found}
    end.

-spec state(room_id()) -> {ok, map()} | {error, room_not_found}.
%% @doc Return a lobby-registered room's complete rules snapshot.
%% Sample usage: `room:state(<<"room-1">>).`
state(RoomId) when is_binary(RoomId) ->
    case lobby:room_pid(RoomId) of
        {ok, Pid} -> ensure_room_state(gen_statem:call(Pid, state));
        {error, _} -> {error, room_not_found}
    end.

%% ------------------------------------------------------------------
%% gen_statem callbacks.
%% ------------------------------------------------------------------

%% @doc Use state function callbacks for setup, active, and finished phases.
%% Sample usage: called by `gen_statem` during room startup.
callback_mode() ->
    state_functions.

-spec init([term()]) -> {ok, rules:phase(), #data{}}.
%% @doc Initialize the room process from supervisor arguments.
%% Sample usage: called by `gen_statem:start_link/3`.
init([RoomId, Rules, Players, Options]) ->
    RulesState = Rules:init(Players, Options),
    PlayersMap = players_from_infos(Players),
    Data = #data{
        room_id = RoomId,
        rules = Rules,
        rules_state = RulesState,
        players = PlayersMap
    },
    notify_players_view(Data, state),
    {ok, rules_phase(Rules, RulesState), Data}.

%% @doc Handle gen_statem events while a rules module reports setup phase.
%% Sample usage: called by OTP for room state events.
setup(EventType, EventContent, Data) ->
    handle_room_event(setup, EventType, EventContent, Data).

%% @doc Handle gen_statem events while a rules module reports active phase.
%% Sample usage: called by OTP for room state events.
active(EventType, EventContent, Data) ->
    handle_room_event(active, EventType, EventContent, Data).

%% @doc Handle gen_statem events while a rules module reports finished phase.
%% Sample usage: called by OTP for room state events.
finished(EventType, EventContent, Data) ->
    handle_room_event(finished, EventType, EventContent, Data).

-spec terminate(term(), rules:phase(), #data{}) -> ok.
%% @doc Room process termination hook.
%% Sample usage: called by OTP during room shutdown.
terminate(_Reason, _StateName, _Data) ->
    ok.

-spec code_change(term(), rules:phase(), #data{}, term()) -> {ok, rules:phase(), #data{}}.
%% @doc Preserve room state during hot-code upgrades.
%% Sample usage: called by OTP release handling.
code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%% ------------------------------------------------------------------
%% Event handling.
%% ------------------------------------------------------------------

handle_room_event(_StateName, {call, From}, state, Data) ->
    Reply = {ok, (Data#data.rules):snapshot(Data#data.rules_state)},
    {keep_state, Data, [{reply, From, Reply}]};
handle_room_event(_StateName, {call, From}, {reconnect, PlayerId, Pid}, Data) ->
    case reconnect_player(PlayerId, Pid, Data) of
        {ok, NewData} ->
            View = player_view(NewData, PlayerId),
            Reply = room_events:reconnect_info(
                opponent_id(PlayerId, NewData#data.players), View
            ),
            {keep_state, NewData, [{reply, From, {ok, Reply}}]};
        {error, Reason} ->
            {keep_state, Data, [{reply, From, {error, Reason}}]}
    end;
handle_room_event(finished, {call, From}, {action, PlayerId, _Action}, Data) ->
    Reason = <<"room_finished">>,
    notify_player(PlayerId, Data#data.players, #{type => <<"error">>, reason => Reason}),
    {keep_state, Data, [{reply, From, {error, Reason}}]};
handle_room_event(_StateName, {call, From}, {action, PlayerId, Action}, Data) ->
    handle_player_action(From, PlayerId, Action, Data);
handle_room_event(_StateName, cast, {leave, PlayerId, Pid}, Data) ->
    handle_player_leave(PlayerId, Pid, Data);
handle_room_event(_StateName, info, {'DOWN', _Ref, process, Pid, _Reason}, Data) ->
    case player_id_by_pid(Pid, Data#data.players) of
        undefined -> {keep_state, Data};
        PlayerId -> handle_player_leave(PlayerId, Pid, Data)
    end;
handle_room_event(_StateName, _EventType, _EventContent, Data) ->
    {keep_state, Data}.

%% ------------------------------------------------------------------
%% Private helpers.
%% ------------------------------------------------------------------

-spec handle_player_action({pid(), term()}, player_id_bin(), map(), #data{}) ->
    gen_statem:state_function_result(rules:phase()).
handle_player_action(From, PlayerId, Action, Data) ->
    case maps:is_key(PlayerId, Data#data.players) of
        false ->
            Reason = <<"unknown_player">>,
            {keep_state, Data, [{reply, From, {error, Reason}}]};
        true ->
            case action_allowed(Data, PlayerId, Action) of
                true -> apply_player_action(From, PlayerId, Action, Data);
                false -> reject_player_action(From, PlayerId, <<"action_not_allowed">>, Data)
            end
    end.

-spec apply_player_action({pid(), term()}, player_id_bin(), map(), #data{}) ->
    gen_statem:state_function_result(rules:phase()).
apply_player_action(From, PlayerId, Action, Data) ->
    Event = action_event(PlayerId, Action),
    case (Data#data.rules):handle_event(Event, Data#data.rules_state) of
        {ok, RulesState, _Events} ->
            NewData = Data#data{rules_state = RulesState},
            notify_players_view(NewData, update),
            NextState = rules_phase(NewData#data.rules, RulesState),
            {next_state, NextState, NewData, [{reply, From, ok}]};
        {error, Reason} ->
            reject_player_action(From, PlayerId, Reason, Data)
    end.

-spec reject_player_action({pid(), term()}, player_id_bin(), binary(), #data{}) ->
    gen_statem:state_function_result(rules:phase()).
reject_player_action(From, PlayerId, Reason, Data) ->
    notify_player(PlayerId, Data#data.players, #{
        type => <<"error">>,
        reason => Reason
    }),
    {keep_state, Data, [{reply, From, {error, Reason}}]}.

-spec action_allowed(#data{}, player_id_bin(), map()) -> boolean().
action_allowed(Data, PlayerId, Action) ->
    AllowedActions = (Data#data.rules):allowed_actions(Data#data.rules_state, PlayerId),
    action:allowed(Action, AllowedActions).

-spec action_event(player_id_bin(), map()) -> rules:event().
action_event(PlayerId, Action) ->
    action:event(PlayerId, Action).

-spec ensure_player_info(player_info()) -> player_info().
ensure_player_info(#{pid := Pid, id := Id, name := Name} = PlayerInfo) when
    is_pid(Pid), is_binary(Id), is_binary(Name)
->
    PlayerInfo.

-spec players_from_infos([player_info()]) -> players_map().
players_from_infos(PlayerInfos) ->
    lists:foldl(
        fun(PlayerInfo, Acc) ->
            Pid = maps:get(pid, PlayerInfo),
            Ref = erlang:monitor(process, Pid),
            PlayerId = maps:get(id, PlayerInfo),
            Acc#{PlayerId => PlayerInfo#{ref => Ref}}
        end,
        #{},
        PlayerInfos
    ).

-spec rules_phase(rules_module(), rules:state()) -> rules:phase().
rules_phase(Rules, RulesState) ->
    Rules:phase(RulesState).

-spec handle_player_leave(player_id_bin(), pid() | undefined, #data{}) ->
    gen_statem:state_function_result(rules:phase()).
handle_player_leave(PlayerId, Pid, Data) ->
    case player_matches_pid(PlayerId, Pid, Data#data.players) of
        true ->
            notify_other_players(PlayerId, Data#data.players, #{
                type => <<"opponent_left">>,
                room_id => Data#data.room_id
            }),
            {keep_state, mark_player_disconnected(PlayerId, Data)};
        false ->
            {keep_state, Data}
    end.

-spec player_matches_pid(player_id_bin(), pid() | undefined, players_map()) -> boolean().
player_matches_pid(PlayerId, undefined, Players) ->
    maps:is_key(PlayerId, Players);
player_matches_pid(PlayerId, Pid, Players) when is_pid(Pid) ->
    case maps:get(PlayerId, Players, undefined) of
        #{pid := Pid} -> true;
        _ -> false
    end.

-spec reconnect_player(player_id_bin(), pid(), #data{}) ->
    {ok, #data{}} | {error, unknown_player}.
reconnect_player(PlayerId, Pid, Data) ->
    case maps:get(PlayerId, Data#data.players, undefined) of
        undefined ->
            {error, unknown_player};
        Player ->
            demonitor_player(Player),
            Ref = erlang:monitor(process, Pid),
            UpdatedPlayer = Player#{pid => Pid, ref => Ref},
            Players = (Data#data.players)#{PlayerId => UpdatedPlayer},
            {ok, Data#data{players = Players}}
    end.

-spec mark_player_disconnected(player_id_bin(), #data{}) -> #data{}.
mark_player_disconnected(PlayerId, Data) ->
    case maps:get(PlayerId, Data#data.players, undefined) of
        undefined ->
            Data;
        Player ->
            demonitor_player(Player),
            Players = (Data#data.players)#{
                PlayerId => Player#{pid => undefined, ref => undefined}
            },
            Data#data{players = Players}
    end.

-spec demonitor_player(player_entry()) -> ok.
demonitor_player(#{ref := Ref}) when is_reference(Ref) ->
    erlang:demonitor(Ref, [flush]),
    ok;
demonitor_player(_) ->
    ok.

-spec player_id_by_pid(pid(), players_map()) -> player_id_bin() | undefined.
player_id_by_pid(_Pid, Players) when map_size(Players) =:= 0 ->
    undefined;
player_id_by_pid(Pid, Players) ->
    Matches = [
        Id
     || {Id, #{pid := PlayerPid}} <- maps:to_list(Players),
        is_pid(PlayerPid),
        PlayerPid =:= Pid
    ],
    case Matches of
        [Id | _] -> Id;
        _ -> undefined
    end.

-spec opponent_id(player_id_bin(), players_map()) -> player_id_bin().
opponent_id(PlayerId, Players) ->
    case maps:keys(maps:remove(PlayerId, Players)) of
        [OpponentId | _] -> OpponentId;
        [] -> <<>>
    end.

-spec notify_players_view(#data{}, state | update) -> ok.
notify_players_view(Data, Event) ->
    notify_players_with(
        Data#data.players,
        fun(PlayerId) ->
            room_event(Event, Data#data.room_id, player_view(Data, PlayerId))
        end
    ).

-spec player_view(#data{}, player_id_bin()) -> protocol_view().
player_view(Data, PlayerId) ->
    View = (Data#data.rules):public_view(Data#data.rules_state, PlayerId),
    Actions = (Data#data.rules):allowed_actions(Data#data.rules_state, PlayerId),
    View#{allowed_actions => Actions}.

-spec room_event(state | update, room_id(), map()) -> map().
room_event(state, RoomId, View) ->
    room_events:state(RoomId, View);
room_event(update, RoomId, View) ->
    room_events:update(RoomId, View).

-spec notify_players_with(players_map(), fun((player_id_bin()) -> map())) -> ok.
notify_players_with(Players, PayloadFun) ->
    lists:foreach(
        fun
            ({PlayerId, #{pid := Pid}}) when is_pid(Pid) ->
                Pid ! {socket_send, PayloadFun(PlayerId)};
            (_) ->
                ok
        end,
        maps:to_list(Players)
    ),
    ok.

-spec notify_other_players(player_id_bin(), players_map(), map()) -> ok.
notify_other_players(LeavingPlayerId, Players, Payload) ->
    OtherPlayers = maps:remove(LeavingPlayerId, Players),
    notify_players(OtherPlayers, Payload).

-spec notify_players(players_map(), map()) -> ok.
notify_players(Players, Payload) ->
    lists:foreach(
        fun
            (#{pid := Pid}) when is_pid(Pid) -> Pid ! {socket_send, Payload};
            (_) -> ok
        end,
        maps:values(Players)
    ),
    ok.

-spec notify_player(player_id_bin(), players_map(), map()) -> ok.
notify_player(PlayerId, Players, Payload) ->
    case maps:get(PlayerId, Players, undefined) of
        undefined ->
            ok;
        #{pid := Pid} when is_pid(Pid) ->
            Pid ! {socket_send, Payload},
            ok;
        _ ->
            ok
    end,
    ok.

-spec ensure_action_result(ok | {error, binary()}) -> ok | {error, binary()}.
ensure_action_result(ok) ->
    ok;
ensure_action_result({error, Reason}) when is_binary(Reason) ->
    {error, Reason}.

-spec ensure_room_state({ok, map()}) -> {ok, map()}.
ensure_room_state({ok, RoomState}) when is_map(RoomState) ->
    {ok, RoomState}.

-spec ensure_reconnect_result({ok, map()} | {error, unknown_player}) ->
    {ok, map()} | {error, unknown_player}.
ensure_reconnect_result({ok, RoomInfo}) when is_map(RoomInfo) ->
    {ok, RoomInfo};
ensure_reconnect_result({error, unknown_player}) ->
    {error, unknown_player}.
