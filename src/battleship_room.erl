-module(battleship_room).
-behaviour(gen_server).

%% @doc Game room process that owns a single battleship match.

-include_lib("battleship/include/battleship.hrl").

-export([start_link/3, move/4, leave/2, leave/3, reconnect/3, game_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type room_id() :: binary().
-type player_id_bin() :: binary().
-type player_info() :: #{
    pid := pid(),
    id := player_id_bin(),
    name := binary(),
    board := board()
}.
-type player_entry() :: #{
    pid := pid() | undefined,
    id := player_id_bin(),
    name := binary(),
    board := board(),
    ref := reference() | undefined
}.
-type players_map() :: #{player_id_bin() => player_entry()}.
-type move_result() :: ok | {error, binary() | room_not_found}.

-record(state, {
    room_id :: room_id(),
    game :: #game{},
    players = #{} :: players_map()
}).

%% ------------------------------------------------------------------
%% Public API.
%% ------------------------------------------------------------------

-spec start_link(room_id(), player_info(), player_info()) -> {ok, pid()} | ignore | {error, term()}.
start_link(RoomId, Player1, Player2) when is_binary(RoomId) ->
    gen_server:start_link(
        ?MODULE, [RoomId, ensure_player_info(Player1), ensure_player_info(Player2)], []
    ).

-spec move(room_id(), player_id_bin(), integer(), integer()) -> move_result().
move(RoomId, PlayerId, Row, Column) when
    is_binary(RoomId), is_binary(PlayerId), is_integer(Row), is_integer(Column)
->
    case battleship_lobby:room_pid(RoomId) of
        {ok, Pid} -> ensure_move_result(gen_server:call(Pid, {move, PlayerId, Row, Column}));
        {error, _} -> {error, room_not_found}
    end.

-spec leave(room_id(), player_id_bin()) -> ok.
leave(RoomId, PlayerId) when is_binary(RoomId), is_binary(PlayerId) ->
    leave(RoomId, PlayerId, undefined).

-spec leave(room_id(), player_id_bin(), pid() | undefined) -> ok.
leave(RoomId, PlayerId, Pid) when is_binary(RoomId), is_binary(PlayerId) ->
    case battleship_lobby:room_pid(RoomId) of
        {ok, RoomPid} -> gen_server:cast(RoomPid, {leave, PlayerId, Pid});
        {error, _} -> ok
    end.

-spec reconnect(room_id(), player_id_bin(), pid()) ->
    {ok, #{game := map(), opponent_id := player_id_bin()}} | {error, room_not_found | unknown_player}.
reconnect(RoomId, PlayerId, Pid) when is_binary(RoomId), is_binary(PlayerId), is_pid(Pid) ->
    case battleship_lobby:room_pid(RoomId) of
        {ok, RoomPid} -> ensure_reconnect_result(gen_server:call(RoomPid, {reconnect, PlayerId, Pid}));
        {error, _} -> {error, room_not_found}
    end.

-spec game_state(room_id()) -> {ok, #game{}} | {error, room_not_found}.
game_state(RoomId) when is_binary(RoomId) ->
    case battleship_lobby:room_pid(RoomId) of
        {ok, Pid} -> ensure_game_state(gen_server:call(Pid, state));
        {error, _} -> {error, room_not_found}
    end.

%% ------------------------------------------------------------------
%% gen_server callbacks.
%% ------------------------------------------------------------------

-spec init([room_id() | player_info()]) -> {ok, #state{}}.
init([RoomId, Player1, Player2]) ->
    Game = init_game(Player1, Player2),
    Players = players_from_infos([Player1, Player2]),
    notify_players(Players, #{
        type => <<"game_state">>,
        room_id => RoomId,
        game => game_to_map(Game)
    }),
    {ok, #state{room_id = RoomId, game = Game, players = Players}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, ok | {ok, #game{}} | {error, binary() | unknown_request}, #state{}}.
handle_call(state, _From, State) ->
    {reply, {ok, State#state.game}, State};
handle_call({reconnect, PlayerId, Pid}, _From, State) ->
    case reconnect_player(PlayerId, Pid, State) of
        {ok, NewState} ->
            Reply = #{
                game => game_to_map(NewState#state.game),
                opponent_id => opponent_id(NewState#state.game, PlayerId)
            },
            {reply, {ok, Reply}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({move, PlayerId, Row, Column}, _From, State) ->
    case can_move(PlayerId, Row, Column, State) of
        ok ->
            case do_move(PlayerId, Row, Column, State#state.game) of
                {ok, NewGame} ->
                    notify_players(State#state.players, #{
                        type => <<"game_update">>,
                        room_id => State#state.room_id,
                        game => game_to_map(NewGame)
                    }),
                    {reply, ok, State#state{game = NewGame}};
                {error, Reason} ->
                    notify_player(PlayerId, State#state.players, #{
                        type => <<"error">>,
                        reason => Reason
                    }),
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            notify_player(PlayerId, State#state.players, #{
                type => <<"error">>,
                reason => Reason
            }),
            {reply, {error, Reason}, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({leave, PlayerId, Pid}, State) ->
    handle_player_leave(PlayerId, Pid, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case player_id_by_pid(Pid, State#state.players) of
        undefined -> {noreply, State};
        PlayerId -> handle_player_leave(PlayerId, Pid, State)
    end;
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

-spec ensure_player_info(player_info()) -> player_info().
ensure_player_info(#{pid := Pid, id := Id, name := Name, board := Board} = PlayerInfo) when
    is_pid(Pid), is_binary(Id), is_binary(Name), is_list(Board)
->
    PlayerInfo.

-spec init_game(player_info(), player_info()) -> #game{}.
init_game(Player1, Player2) ->
    PlayerOne = #player{id = maps:get(id, Player1), board = maps:get(board, Player1)},
    PlayerTwo = #player{id = maps:get(id, Player2), board = maps:get(board, Player2)},
    #game{
        player_one = PlayerOne,
        player_two = PlayerTwo,
        first_turn = battleship_utils:get_random_binary(PlayerOne#player.id, PlayerTwo#player.id),
        turns = [],
        state = 'ACTIVE'
    }.

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

-spec can_move(player_id_bin(), integer(), integer(), #state{}) -> ok | {error, binary()}.
can_move(PlayerId, Row, Column, State) ->
    case maps:is_key(PlayerId, State#state.players) of
        false ->
            {error, <<"unknown_player">>};
        true ->
            case State#state.game#game.state of
                'FINISHED' ->
                    {error, <<"game_finished">>};
                _ ->
                    case valid_coords(Row, Column) of
                        false ->
                            {error, <<"invalid_coordinates">>};
                        true ->
                            case current_turn_id(State#state.game) =:= PlayerId of
                                true -> ok;
                                false -> {error, <<"not_your_turn">>}
                            end
                    end
            end
    end.

-spec do_move(player_id_bin(), integer(), integer(), #game{}) -> {ok, #game{}} | {error, binary()}.
do_move(_PlayerId, Row, Column, Game) ->
    {Row1, Col1} = to_board_coords(Row, Column),
    try
        {ok, battleship_game:next_move(Game, Row1, Col1)}
    catch
        _:_ -> {error, <<"invalid_move">>}
    end.

-spec handle_player_leave(player_id_bin(), pid() | undefined, #state{}) ->
    {noreply, #state{}}.
handle_player_leave(PlayerId, Pid, State) ->
    case player_matches_pid(PlayerId, Pid, State#state.players) of
        true ->
            notify_other_players(PlayerId, State#state.players, #{
                type => <<"opponent_left">>,
                room_id => State#state.room_id
            }),
            {noreply, mark_player_disconnected(PlayerId, State)};
        false ->
            {noreply, State}
    end.

-spec player_matches_pid(player_id_bin(), pid() | undefined, players_map()) -> boolean().
player_matches_pid(PlayerId, undefined, Players) ->
    maps:is_key(PlayerId, Players);
player_matches_pid(PlayerId, Pid, Players) when is_pid(Pid) ->
    case maps:get(PlayerId, Players, undefined) of
        #{pid := Pid} -> true;
        _ -> false
    end.

-spec reconnect_player(player_id_bin(), pid(), #state{}) ->
    {ok, #state{}} | {error, unknown_player}.
reconnect_player(PlayerId, Pid, State) ->
    case maps:get(PlayerId, State#state.players, undefined) of
        undefined ->
            {error, unknown_player};
        Player ->
            demonitor_player(Player),
            Ref = erlang:monitor(process, Pid),
            UpdatedPlayer = Player#{pid => Pid, ref => Ref},
            Players = (State#state.players)#{PlayerId => UpdatedPlayer},
            {ok, State#state{players = Players}}
    end.

-spec mark_player_disconnected(player_id_bin(), #state{}) -> #state{}.
mark_player_disconnected(PlayerId, State) ->
    case maps:get(PlayerId, State#state.players, undefined) of
        undefined ->
            State;
        Player ->
            demonitor_player(Player),
            Players = (State#state.players)#{PlayerId => Player#{pid => undefined, ref => undefined}},
            State#state{players = Players}
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

-spec current_turn_id(#game{}) -> player_id_bin().
current_turn_id(Game) ->
    case Game#game.turns of
        [] ->
            Game#game.first_turn;
        [#strike{id = PlayerId, res = 'HIT'} | _] ->
            PlayerId;
        [Last | _] ->
            opponent_id(Game, Last#strike.id)
    end.

-spec opponent_id(#game{}, player_id_bin()) -> player_id_bin().
opponent_id(Game, PlayerId) ->
    case PlayerId =:= Game#game.player_one#player.id of
        true -> Game#game.player_two#player.id;
        false -> Game#game.player_one#player.id
    end.

-spec valid_coords(integer(), integer()) -> boolean().
valid_coords(Row, Column) when is_integer(Row), is_integer(Column) ->
    Row >= 0 andalso Row =< 9 andalso Column >= 0 andalso Column =< 9;
valid_coords(_, _) ->
    false.

-spec to_board_coords(integer(), integer()) -> {row(), column()}.
to_board_coords(Row, Column) ->
    {Row + 1, Column + 1}.

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

-spec notify_other_players(player_id_bin(), players_map(), map()) -> ok.
notify_other_players(LeavingPlayerId, Players, Payload) ->
    OtherPlayers = maps:remove(LeavingPlayerId, Players),
    notify_players(OtherPlayers, Payload).

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

-spec game_to_map(#game{}) -> map().
game_to_map(Game) ->
    #{
        player_one => player_to_map(Game#game.player_one),
        player_two => player_to_map(Game#game.player_two),
        first_turn => Game#game.first_turn,
        current_turn => current_turn_for_map(Game),
        winner => winner_id(Game),
        phase => phase(Game),
        turns => [strike_to_map(Strike) || Strike <- Game#game.turns],
        state => Game#game.state
    }.

-spec current_turn_for_map(#game{}) -> player_id_bin() | null.
current_turn_for_map(Game) ->
    case Game#game.state of
        'FINISHED' -> null;
        _ -> current_turn_id(Game)
    end.

-spec winner_id(#game{}) -> player_id_bin() | null.
winner_id(#game{state = 'FINISHED', turns = [#strike{id = PlayerId} | _]}) ->
    PlayerId;
winner_id(_) ->
    null.

-spec phase(#game{}) -> binary().
phase(#game{state = 'FINISHED'}) ->
    <<"finished">>;
phase(_) ->
    <<"playing">>.

-spec player_to_map(#player{}) -> map().
player_to_map(#player{id = Id, board = Board}) ->
    #{id => Id, board => serialize_board(Board)}.

-spec strike_to_map(#strike{}) -> map().
strike_to_map(#strike{id = Id, x = X, y = Y, res = Res}) ->
    #{id => Id, x => X, y => Y, res => Res}.

-spec serialize_board(board()) -> [[binary()]].
serialize_board(Board) ->
    [[cell_to_binary(Cell) || Cell <- Row] || Row <- Board].

-spec cell_to_binary(grid_state()) -> binary().
cell_to_binary(Cell) when is_atom(Cell) ->
    atom_to_binary(Cell, utf8).
-spec ensure_move_result(ok | {error, binary()}) -> ok | {error, binary()}.
ensure_move_result(ok) ->
    ok;
ensure_move_result({error, Reason}) when is_binary(Reason) ->
    {error, Reason}.

-spec ensure_game_state({ok, #game{}}) -> {ok, #game{}}.
ensure_game_state({ok, Game = #game{}}) ->
    {ok, Game}.

-spec ensure_reconnect_result({ok, map()} | {error, unknown_player}) ->
    {ok, map()} | {error, unknown_player}.
ensure_reconnect_result({ok, GameInfo}) when is_map(GameInfo) ->
    {ok, GameInfo};
ensure_reconnect_result({error, unknown_player}) ->
    {error, unknown_player}.
