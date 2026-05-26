-module(battleship_room).
-behaviour(gen_server).

%% @doc Game room process that owns a single battleship match.

-include_lib("battleship/include/battleship.hrl").

-export([start_link/3, move/4, leave/2, game_state/1]).
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
    pid := pid(),
    id := player_id_bin(),
    name := binary(),
    board := board(),
    ref := reference()
}.
-type players_map() :: #{player_id_bin() => player_entry()}.
-type move_result() :: ok | {error, binary()}.

-record(state, {
    room_id :: room_id(),
    game :: #game{},
    players = #{} :: players_map()
}).

%% ------------------------------------------------------------------
%% Public API.
%% ------------------------------------------------------------------

-spec start_link(room_id(), player_info(), player_info()) -> {ok, pid()} | {error, term()}.
start_link(RoomId, Player1, Player2) ->
    gen_server:start_link(?MODULE, [RoomId, Player1, Player2], []).

-spec move(room_id(), player_id_bin(), integer(), integer()) -> move_result().
move(RoomId, PlayerId, Row, Column) ->
    case battleship_lobby:room_pid(RoomId) of
        {ok, Pid} -> gen_server:call(Pid, {move, PlayerId, Row, Column});
        {error, _} -> {error, room_not_found}
    end.

-spec leave(room_id(), player_id_bin()) -> ok.
leave(RoomId, PlayerId) ->
    case battleship_lobby:room_pid(RoomId) of
        {ok, Pid} -> gen_server:cast(Pid, {leave, PlayerId});
        {error, _} -> ok
    end.

-spec game_state(room_id()) -> {ok, #game{}} | {error, room_not_found}.
game_state(RoomId) ->
    case battleship_lobby:room_pid(RoomId) of
        {ok, Pid} -> gen_server:call(Pid, state);
        {error, _} -> {error, room_not_found}
    end.

%% ------------------------------------------------------------------
%% gen_server callbacks.
%% ------------------------------------------------------------------

-spec init(list()) -> {ok, #state{}}.
init([RoomId, Player1, Player2]) ->
    Game = init_game(Player1, Player2),
    Players = players_from_infos([Player1, Player2]),
    notify_players(Players, #{
        type => <<"game_state">>,
        room_id => RoomId,
        game => game_to_map(Game)
    }),
    {ok, #state{room_id = RoomId, game = Game, players = Players}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(state, _From, State) ->
    {reply, {ok, State#state.game}, State};
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

-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_cast({leave, PlayerId}, State) ->
    handle_player_leave(PlayerId, State);
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case player_id_by_pid(Pid, State#state.players) of
        undefined -> {noreply, State};
        PlayerId -> handle_player_leave(PlayerId, State)
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

-spec handle_player_leave(player_id_bin(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.
handle_player_leave(PlayerId, State) ->
    Remaining = maps:remove(PlayerId, State#state.players),
    notify_players(Remaining, #{
        type => <<"opponent_left">>,
        room_id => State#state.room_id
    }),
    case maps:size(Remaining) of
        0 -> {stop, normal, State#state{players = Remaining}};
        _ -> {noreply, State#state{players = Remaining}}
    end.

-spec player_id_by_pid(pid(), players_map()) -> player_id_bin() | undefined.
player_id_by_pid(_Pid, Players) when map_size(Players) =:= 0 ->
    undefined;
player_id_by_pid(Pid, Players) ->
    Matches = [Id || {Id, #{pid := PlayerPid}} <- maps:to_list(Players), PlayerPid =:= Pid],
    case Matches of
        [Id | _] -> Id;
        _ -> undefined
    end.

-spec current_turn_id(#game{}) -> player_id_bin().
current_turn_id(Game) ->
    case Game#game.turns of
        [] ->
            Game#game.first_turn;
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

-spec to_board_coords(integer(), integer()) -> {integer(), integer()}.
to_board_coords(Row, Column) ->
    {Row + 1, Column + 1}.

-spec notify_players(players_map(), map()) -> ok.
notify_players(Players, Payload) ->
    lists:foreach(
        fun(#{pid := Pid}) -> Pid ! {socket_send, Payload} end,
        maps:values(Players)
    ),
    ok.

-spec notify_player(player_id_bin(), players_map(), map()) -> ok.
notify_player(PlayerId, Players, Payload) ->
    case maps:get(PlayerId, Players, undefined) of
        undefined -> ok;
        #{pid := Pid} -> Pid ! {socket_send, Payload}
    end,
    ok.

-spec game_to_map(#game{}) -> map().
game_to_map(Game) ->
    #{
        player_one => player_to_map(Game#game.player_one),
        player_two => player_to_map(Game#game.player_two),
        first_turn => Game#game.first_turn,
        turns => [strike_to_map(Strike) || Strike <- Game#game.turns],
        state => Game#game.state
    }.

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
