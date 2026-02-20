-module(battleship_lobby).
-behaviour(gen_server).

%% @doc Matchmaking lobby that pairs players into game rooms.

-include_lib("battleship/include/battleship.hrl").

-export([start_link/0, join/2, leave/1, list_rooms/0, room_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type room_id() :: binary().
-type player_id_bin() :: binary().
-type player_info() :: #{
    player_id => player_id_bin(),
    name => binary(),
    board => board()
}.
-type join_reply() :: {waiting, player_id_bin()} | {matched, player_id_bin(), player_id_bin(), room_id()}.
-type room_entry() :: #{pid := pid(), ref := reference()}.

-record(lobby_player, {
    pid :: pid(),
    ref :: reference(),
    player_id :: player_id_bin(),
    name :: binary(),
    board :: board()
}).
-record(state, {
    waiting = [] :: [#lobby_player{}],
    rooms = #{} :: #{room_id() => room_entry()},
    counter = 0 :: non_neg_integer()
}).

%% ------------------------------------------------------------------
%% Public API.
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec join(pid(), player_info()) -> join_reply().
join(Pid, PlayerInfo) ->
    gen_server:call(?MODULE, {join, Pid, PlayerInfo}).

-spec leave(pid()) -> ok.
leave(Pid) ->
    gen_server:cast(?MODULE, {leave, Pid}).

-spec list_rooms() -> [room_id()].
list_rooms() ->
    gen_server:call(?MODULE, list_rooms).

-spec room_pid(room_id()) -> {ok, pid()} | {error, not_found}.
room_pid(RoomId) ->
    gen_server:call(?MODULE, {room_pid, RoomId}).

%% ------------------------------------------------------------------
%% gen_server callbacks.
%% ------------------------------------------------------------------

-spec init(list()) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(list_rooms, _From, State) ->
    RoomIds = maps:keys(State#state.rooms),
    {reply, RoomIds, State};
handle_call({room_pid, RoomId}, _From, State) ->
    case maps:get(RoomId, State#state.rooms, undefined) of
        #{pid := Pid} -> {reply, {ok, Pid}, State};
        _ -> {reply, {error, not_found}, State}
    end;
handle_call({join, Pid, PlayerInfo}, _From, State) ->
    Player = build_player(Pid, PlayerInfo),
    case State#state.waiting of
        [] ->
            NewState = State#state{waiting = [Player | State#state.waiting]},
            {reply, {waiting, Player#lobby_player.player_id}, NewState};
        [Waiting | Rest] ->
            erlang:demonitor(Waiting#lobby_player.ref, [flush]),
            erlang:demonitor(Player#lobby_player.ref, [flush]),
            RoomId = make_room_id(State#state.counter + 1),
            Player1 = lobby_player_to_room_player(Waiting),
            Player2 = lobby_player_to_room_player(Player),
            {ok, RoomPid} = battleship_room_sup:start_room(RoomId, Player1, Player2),
            RoomRef = erlang:monitor(process, RoomPid),
            Rooms = (State#state.rooms)#{RoomId => #{pid => RoomPid, ref => RoomRef}},
            notify_player_match(Waiting, Player#lobby_player.player_id, RoomId),
            {reply, {matched, Player#lobby_player.player_id, Waiting#lobby_player.player_id, RoomId}, State#state{
                waiting = Rest,
                rooms = Rooms,
                counter = State#state.counter + 1
            }}
    end;
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({leave, Pid}, State) ->
    NewWaiting = [Player || Player <- State#state.waiting, Player#lobby_player.pid =/= Pid],
    {noreply, State#state{waiting = NewWaiting}};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    NewWaiting = [Player || Player <- State#state.waiting, Player#lobby_player.ref =/= Ref],
    Rooms = remove_room_by_ref(Ref, State#state.rooms),
    {noreply, State#state{waiting = NewWaiting, rooms = Rooms}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Private helpers.
%% ------------------------------------------------------------------

-spec build_player(pid(), player_info()) -> #lobby_player{}.
build_player(Pid, PlayerInfo) ->
    PlayerId = maps:get(player_id, PlayerInfo, make_player_id()),
    Name = maps:get(name, PlayerInfo, <<"player">>),
    Board = maps:get(board, PlayerInfo, battleship_game:place_fleet_random()),
    Ref = erlang:monitor(process, Pid),
    #lobby_player{
        pid = Pid,
        ref = Ref,
        player_id = PlayerId,
        name = Name,
        board = Board
    }.

-spec make_player_id() -> player_id_bin().
make_player_id() ->
    integer_to_binary(erlang:unique_integer([monotonic, positive])).

-spec make_room_id(non_neg_integer()) -> room_id().
make_room_id(Counter) ->
    <<<<"room-">>/binary, (integer_to_binary(Counter))/binary>>.

-spec lobby_player_to_room_player(#lobby_player{}) -> map().
lobby_player_to_room_player(#lobby_player{pid = Pid, player_id = PlayerId, name = Name, board = Board}) ->
    #{pid => Pid, id => PlayerId, name => Name, board => Board}.

-spec notify_player_match(#lobby_player{}, player_id_bin(), room_id()) -> ok.
notify_player_match(#lobby_player{pid = Pid, player_id = PlayerId, name = Name}, OpponentId, RoomId) ->
    Payload = #{
        type => <<"match_found">>,
        room_id => RoomId,
        player_id => PlayerId,
        opponent_id => OpponentId,
        player_name => Name
    },
    Pid ! {socket_send, Payload},
    ok.

-spec remove_room_by_ref(reference(), #{room_id() => room_entry()}) -> #{room_id() => room_entry()}.
remove_room_by_ref(Ref, Rooms) ->
    maps:filter(
        fun(_RoomId, RoomInfo) ->
            maps:get(ref, RoomInfo) =/= Ref
        end,
        Rooms
    ).
