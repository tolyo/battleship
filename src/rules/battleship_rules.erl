-module(battleship_rules).
%% @doc Battleship rule implementation for the generic room state machine.
-behaviour(rules).

-export([
    init/2,
    handle_event/2,
    phase/1,
    prepare_player/1,
    public_view/2,
    allowed_actions/2,
    snapshot/1,
    place_units_random/0,
    init_mock_match/0,
    get_opposite_player/2,
    get_player_by_id/2,
    next_move/3
]).
-include_lib("battleship/include/rules/battleship.hrl").

%% ------------------------------------------------------------------
%% Public API.
%% ------------------------------------------------------------------

-spec prepare_player(map()) -> map().
%% @doc Normalize Battleship-specific player setup before the room starts.
%% Sample usage: `Prepared = battleship_rules:prepare_player(#{id => <<"p1">>, board => Json}).`
prepare_player(PlayerInfo) ->
    Board = parse_board(player_value(board, <<"board">>, PlayerInfo, undefined)),
    PlayerInfo#{board => Board}.

-spec init([map()], map()) -> map().
%% @doc Initialize the Battleship rules model for two prepared players.
%% Sample usage: `State = battleship_rules:init([P1, P2], #{}).`
init([Player1, Player2], _Options) ->
    PlayerOne = #player{id = maps:get(id, Player1), board = maps:get(board, Player1)},
    PlayerTwo = #player{id = maps:get(id, Player2), board = maps:get(board, Player2)},
    Match = #match{
        player_one = PlayerOne,
        player_two = PlayerTwo,
        first_turn = utils:get_random_binary(PlayerOne#player.id, PlayerTwo#player.id),
        turns = [],
        state = 'ACTIVE'
    },
    #{model => Match}.

-spec handle_event(map(), map()) -> {ok, map(), [map()]} | {error, binary()}.
%% @doc Apply a Battleship rules event to the current model.
%% Sample usage: `battleship_rules:handle_event(#{type => <<"move">>, player_id => Id, payload => #{row => 0, column => 0}}, State).`
handle_event(
    #{type := <<"move">>, player_id := PlayerId, payload := #{row := Row, column := Column}},
    State = #{model := Match}
) ->
    case can_move(PlayerId, Row, Column, Match) of
        ok ->
            {Row1, Col1} = to_board_coords(Row, Column),
            try
                NewMatch = next_move(Match, Row1, Col1),
                {ok, State#{model => NewMatch}, [#{type => state_changed}]}
            catch
                _:_ -> {error, <<"invalid_move">>}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
handle_event(_Event, _State) ->
    {error, <<"unknown_event">>}.

-spec phase(map()) -> rules:phase().
%% @doc Report the generic room phase for the current Battleship model.
%% Sample usage: `Phase = battleship_rules:phase(State).`
phase(#{model := #match{state = 'FINISHED'}}) ->
    finished;
phase(_State) ->
    active.

-spec public_view(map(), player_id()) -> map().
%% @doc Return the Battleship board view visible to a player.
%% Sample usage: `View = battleship_rules:public_view(State, <<"p1">>).`
public_view(#{model := Match}, PlayerId) ->
    match_to_public_map(Match, PlayerId).

-spec allowed_actions(map(), player_id()) -> [map()].
%% @doc Return the Battleship actions the player may currently submit.
%% Sample usage: `Actions = battleship_rules:allowed_actions(State, <<"p1">>).`
allowed_actions(#{model := Match}, PlayerId) ->
    allowed_actions_for_model(Match, PlayerId).

-spec snapshot(map()) -> map().
%% @doc Convert the complete Battleship model to a serializable map.
%% Sample usage: `Snapshot = battleship_rules:snapshot(State).`
snapshot(#{model := Match}) ->
    match_to_map(Match).

-spec place_units_random() -> board().
%% @doc Generate a legal random Battleship board.
%% Sample usage: `Board = battleship_rules:place_units_random().`
place_units_random() ->
    try
        place_units(battleship_board:init_board(), battleship_unit:all())
    catch
        _:_ -> place_units_random()
    end.

-spec parse_board(term()) -> board().
parse_board(undefined) ->
    place_units_random();
parse_board(<<>>) ->
    place_units_random();
parse_board(BoardParam) when is_binary(BoardParam) ->
    try
        Board = json:decode(BoardParam),
        case normalize_board(Board) of
            {ok, Normalized} -> Normalized;
            {error, _} -> place_units_random()
        end
    catch
        _:_ -> place_units_random()
    end;
parse_board(Board) ->
    case normalize_board(Board) of
        {ok, Normalized} -> Normalized;
        {error, _} -> place_units_random()
    end.

-spec player_value(atom(), binary(), map(), term()) -> term().
player_value(AtomKey, BinaryKey, PlayerInfo, Default) ->
    case maps:find(AtomKey, PlayerInfo) of
        {ok, Value} -> Value;
        error -> maps:get(BinaryKey, PlayerInfo, Default)
    end.

-spec init_mock_match() -> #match{}.
%% @doc Build a standalone randomized Battleship model for tests or manual checks.
%% Sample usage: `Match = battleship_rules:init_mock_match().`
init_mock_match() ->
    Player1 = #player{id = <<"1">>, board = place_units_random()},
    Player2 = #player{id = <<"2">>, board = place_units_random()},
    #match{
        player_one = Player1,
        player_two = Player2,
        first_turn = utils:get_random_binary(Player1#player.id, Player2#player.id),
        turns = [],
        state = 'ACTIVE'
    }.

-spec get_player_by_id(#match{}, player_id()) -> #player{}.
%% @doc Fetch one player record from a Battleship model by id.
%% Sample usage: `Player = battleship_rules:get_player_by_id(Match, <<"p1">>).`
get_player_by_id(Match, Id) when is_binary(Id) ->
    case Id =:= Match#match.player_one#player.id of
        true -> Match#match.player_one;
        false -> Match#match.player_two
    end.

-spec get_opposite_player(#match{}, #player{}) -> #player{}.
%% @doc Fetch the opponent for a player in a Battleship model.
%% Sample usage: `Opponent = battleship_rules:get_opposite_player(Match, Player).`
get_opposite_player(Match, Player) ->
    case Player#player.id =:= Match#match.player_one#player.id of
        true -> Match#match.player_two;
        false -> Match#match.player_one
    end.

-spec next_move(#match{}, row(), column()) -> #match{}.
%% @doc Apply a validated one-based strike coordinate to a Battleship model.
%% Sample usage: `NextMatch = battleship_rules:next_move(Match, 1, 1).`
next_move(Match, Row, Column) ->
    CurrentPlayer = current_player(Match),
    OppositePlayer = get_opposite_player(Match, CurrentPlayer),
    Board = OppositePlayer#player.board,
    case strike(Board, Row, Column) of
        {'MISS', NewBoard} ->
            update_match(Match, CurrentPlayer, OppositePlayer, NewBoard, 'MISS', Row, Column);
        {'ERROR', _} ->
            error("Wrong move");
        {HitVal, NewBoard} ->
            case battleship_board:count(NewBoard, ?HIT) == battleship_unit:total_size() of
                true ->
                    update_match(
                        Match#match{state = 'FINISHED'},
                        CurrentPlayer,
                        OppositePlayer,
                        NewBoard,
                        'HIT',
                        Row,
                        Column
                    );
                false ->
                    HitCount = battleship_board:count(NewBoard, HitVal),
                    BlockedBoard =
                        case HitVal of
                            '9' ->
                                battleship_board:set_adjacents_blocked(NewBoard);
                            '8' ->
                                battleship_board:set_adjacents_blocked(NewBoard);
                            '7' ->
                                battleship_board:set_adjacents_blocked(NewBoard);
                            _ when HitCount =:= 0 ->
                                battleship_board:set_adjacents_blocked(NewBoard);
                            _ ->
                                NewBoard
                        end,
                    update_match(
                        Match, CurrentPlayer, OppositePlayer, BlockedBoard, 'HIT', Row, Column
                    )
            end
    end.

%%% ---------------------------------------------------
%%% Private functions.
%%% ---------------------------------------------------

-spec can_move(player_id(), integer(), integer(), #match{}) -> ok | {error, binary()}.
can_move(PlayerId, Row, Column, Match) ->
    case has_player(Match, PlayerId) of
        false ->
            {error, <<"unknown_player">>};
        true ->
            case Match#match.state of
                'FINISHED' ->
                    {error, <<"match_finished">>};
                _ ->
                    case valid_coords(Row, Column) of
                        false ->
                            {error, <<"invalid_coordinates">>};
                        true ->
                            case current_turn_id(Match) =:= PlayerId of
                                true -> can_strike_target(PlayerId, Row, Column, Match);
                                false -> {error, <<"not_your_turn">>}
                            end
                    end
            end
    end.

-spec has_player(#match{}, player_id()) -> boolean().
has_player(Match, PlayerId) ->
    PlayerId =:= Match#match.player_one#player.id orelse
        PlayerId =:= Match#match.player_two#player.id.

-spec can_strike_target(player_id(), integer(), integer(), #match{}) -> ok | {error, binary()}.
can_strike_target(PlayerId, Row, Column, Match) ->
    CurrentPlayer = get_player_by_id(Match, PlayerId),
    Opponent = get_opposite_player(Match, CurrentPlayer),
    {Row1, Col1} = to_board_coords(Row, Column),
    case battleship_board:get_cell_value(Opponent#player.board, Row1, Col1) of
        ?EMPTY -> ok;
        Cell ->
            case is_unit_cell(Cell) of
                true -> ok;
                false -> {error, <<"invalid_move">>}
            end
    end.

-spec is_unit_cell(grid_state()) -> boolean().
is_unit_cell('0') -> true;
is_unit_cell('1') -> true;
is_unit_cell('2') -> true;
is_unit_cell('3') -> true;
is_unit_cell('4') -> true;
is_unit_cell('5') -> true;
is_unit_cell('6') -> true;
is_unit_cell('7') -> true;
is_unit_cell('8') -> true;
is_unit_cell('9') -> true;
is_unit_cell(_) -> false.

-spec valid_coords(integer(), integer()) -> boolean().
valid_coords(Row, Column) when is_integer(Row), is_integer(Column) ->
    Row >= 0 andalso Row =< 9 andalso Column >= 0 andalso Column =< 9;
valid_coords(_, _) ->
    false.

-spec to_board_coords(integer(), integer()) -> {row(), column()}.
to_board_coords(Row, Column) ->
    {Row + 1, Column + 1}.

-spec current_player(#match{}) -> #player{}.
current_player(Match) ->
    case Match#match.turns of
        [] ->
            get_player_by_id(Match, Match#match.first_turn);
        [#strike{id = PlayerId, res = 'HIT'} | _] ->
            get_player_by_id(Match, PlayerId);
        [#strike{id = PlayerId} | _] ->
            get_opposite_player(Match, get_player_by_id(Match, PlayerId))
    end.

-spec strike(board(), row(), column()) -> {grid_state() | strike_res(), board()}.
strike(Board, Row, Column) ->
    case battleship_board:get_cell_value(Board, Row, Column) of
        ?EMPTY ->
            {'MISS', battleship_board:update_cell_at(Board, Row, Column, ?MISS)};
        ?BLOCKED ->
            {'ERROR', Board};
        ?HIT ->
            {'ERROR', Board};
        ?MISS ->
            {'ERROR', Board};
        _ ->
            {
                battleship_board:get_cell_value(Board, Row, Column),
                battleship_board:update_cell_at(Board, Row, Column, ?HIT)
            }
    end.

-spec update_match(#match{}, #player{}, #player{}, board(), strike_res(), row(), column()) ->
    #match{}.
update_match(Match, CurrentPlayer, OppositePlayer, NewBoard, Result, Row, Column) ->
    Strike = #strike{id = CurrentPlayer#player.id, x = Column, y = Row, res = Result},
    UpdatedMatch = Match#match{turns = [Strike | Match#match.turns]},
    update_player_board(UpdatedMatch, OppositePlayer, NewBoard).

-spec update_player_board(#match{}, #player{}, board()) -> #match{}.
update_player_board(Match, Player, Board) ->
    case Player#player.id =:= Match#match.player_one#player.id of
        true ->
            Match#match{player_one = Player#player{board = Board}};
        false ->
            Match#match{player_two = Player#player{board = Board}}
    end.

-spec try_place_unit_random(board(), #unit{}, non_neg_integer()) -> board().
try_place_unit_random(_, _, 0) ->
    throw("Unable to place unit");
try_place_unit_random(Board, Unit, Count) ->
    {Column, Row, Orientation} = get_random_unit_coordinate(),
    RandomUnit = Unit#unit{row = Row, column = Column, orientation = Orientation},
    Legal = battleship_board:is_legal(Board, RandomUnit),
    case Legal of
        true -> battleship_board:attach_unit(Board, RandomUnit);
        false -> try_place_unit_random(Board, Unit, Count - 1)
    end.

-spec place_units(board(), unit_collection()) -> board().
place_units(Board, []) ->
    Board;
place_units(Board, [H | T]) ->
    NewBoard = try_place_unit_random(Board, H, 100),
    place_units(NewBoard, T).

-spec get_random_unit_coordinate() -> {column(), row(), unit_orientation()}.
get_random_unit_coordinate() ->
    {
        rand:uniform(10),
        rand:uniform(10),
        utils:get_random_binary('VERTICAL', 'HORIZONTAL')
    }.

-spec match_to_map(#match{}) -> map().
match_to_map(Match) ->
    #{
        player_one => player_to_map(Match#match.player_one),
        player_two => player_to_map(Match#match.player_two),
        first_turn => Match#match.first_turn,
        current_turn => current_turn_for_map(Match),
        winner => winner_id(Match),
        phase => phase_name(Match),
        turns => [strike_to_map(Strike) || Strike <- Match#match.turns],
        state => Match#match.state
    }.

-spec match_to_public_map(#match{}, player_id()) -> map().
match_to_public_map(Match, PlayerId) ->
    Viewer = public_viewer(Match, PlayerId),
    Opponent = public_opponent(Match, PlayerId),
    #{
        viewer => PlayerId,
        own_player => Viewer,
        opponent => Opponent,
        phase => phase_name(Match)
    }.

-spec public_viewer(#match{}, player_id()) -> map() | null.
public_viewer(Match, PlayerId) ->
    case has_player(Match, PlayerId) of
        true -> player_to_public_map(get_player_by_id(Match, PlayerId), true);
        false -> null
    end.

-spec public_opponent(#match{}, player_id()) -> map() | null.
public_opponent(Match, PlayerId) ->
    case has_player(Match, PlayerId) of
        true ->
            Player = get_player_by_id(Match, PlayerId),
            player_to_public_map(get_opposite_player(Match, Player), false);
        false ->
            null
    end.

-spec allowed_actions_for_model(#match{}, player_id()) -> [map()].
allowed_actions_for_model(Match, PlayerId) ->
    case has_player(Match, PlayerId) andalso Match#match.state =/= 'FINISHED' of
        true ->
            case current_turn_id(Match) =:= PlayerId of
                true -> [#{action => <<"move">>, target => <<"opponent_board">>}];
                false -> []
            end;
        false ->
            []
    end.

-spec current_turn_for_map(#match{}) -> player_id() | null.
current_turn_for_map(Match) ->
    case Match#match.state of
        'FINISHED' -> null;
        _ -> current_turn_id(Match)
    end.

-spec current_turn_id(#match{}) -> player_id().
current_turn_id(Match) ->
    case Match#match.turns of
        [] ->
            Match#match.first_turn;
        [#strike{id = PlayerId, res = 'HIT'} | _] ->
            PlayerId;
        [Last | _] ->
            opponent_id(Match, Last#strike.id)
    end.

-spec opponent_id(#match{}, player_id()) -> player_id().
opponent_id(Match, PlayerId) ->
    case PlayerId =:= Match#match.player_one#player.id of
        true -> Match#match.player_two#player.id;
        false -> Match#match.player_one#player.id
    end.

-spec winner_id(#match{}) -> player_id() | null.
winner_id(#match{state = 'FINISHED', turns = [#strike{id = PlayerId} | _]}) ->
    PlayerId;
winner_id(_) ->
    null.

-spec phase_name(#match{}) -> binary().
phase_name(#match{state = 'FINISHED'}) ->
    <<"finished">>;
phase_name(_) ->
    <<"playing">>.

-spec player_to_map(#player{}) -> map().
player_to_map(#player{id = Id, board = Board}) ->
    #{id => Id, board => serialize_board(Board)}.

-spec player_to_public_map(#player{}, boolean()) -> map().
player_to_public_map(#player{id = Id, board = Board}, true) ->
    #{id => Id, board => serialize_board(Board)};
player_to_public_map(#player{id = Id, board = Board}, false) ->
    #{id => Id, board => serialize_board(mask_hidden_units(Board))}.

-spec strike_to_map(#strike{}) -> map().
strike_to_map(#strike{id = Id, x = X, y = Y, res = Res}) ->
    #{id => Id, x => X, y => Y, res => Res}.

-spec mask_hidden_units(board()) -> board().
mask_hidden_units(Board) ->
    [[mask_hidden_unit(Cell) || Cell <- Row] || Row <- Board].

-spec mask_hidden_unit(grid_state()) -> grid_state().
mask_hidden_unit(Cell) ->
    case is_unit_cell(Cell) of
        true -> ?EMPTY;
        false -> Cell
    end.

-spec serialize_board(board()) -> [[binary()]].
serialize_board(Board) ->
    [[cell_to_binary(Cell) || Cell <- Row] || Row <- Board].

-spec cell_to_binary(grid_state()) -> binary().
cell_to_binary(Cell) when is_atom(Cell) ->
    atom_to_binary(Cell, utf8).

-spec normalize_board(term()) -> {ok, board()} | {error, invalid_board | invalid_row}.
normalize_board(Board) when is_list(Board), length(Board) =:= 10 ->
    Rows = [normalize_row(Row) || Row <- Board],
    case
        lists:all(
            fun
                ({ok, _}) -> true;
                (_) -> false
            end,
            Rows
        )
    of
        true -> {ok, [Row || {ok, Row} <- Rows]};
        false -> {error, invalid_row}
    end;
normalize_board(_) ->
    {error, invalid_board}.

-spec normalize_row(term()) -> {ok, [grid_state()]} | {error, invalid_row | invalid_cell}.
normalize_row(Row) when is_list(Row), length(Row) =:= 10 ->
    Cells = [normalize_cell(Cell) || Cell <- Row],
    case
        lists:all(
            fun
                ({ok, _}) -> true;
                (_) -> false
            end,
            Cells
        )
    of
        true -> {ok, [Cell || {ok, Cell} <- Cells]};
        false -> {error, invalid_cell}
    end;
normalize_row(_) ->
    {error, invalid_row}.

-spec normalize_cell(term()) -> {ok, grid_state()} | {error, invalid_cell}.
normalize_cell(Cell) when is_binary(Cell) ->
    normalize_cell(binary_to_list(Cell));
normalize_cell(Cell) when is_list(Cell) ->
    case Cell of
        "_" ->
            {ok, ?EMPTY};
        "o" ->
            {ok, ?BLOCKED};
        "x" ->
            {ok, ?MISS};
        "m" ->
            {ok, ?MISS};
        "+" ->
            {ok, ?HIT};
        [Digit] when Digit >= $0, Digit =< $9 ->
            {ok, digit_atom(Digit)};
        _ ->
            {error, invalid_cell}
    end;
normalize_cell(Cell) when is_integer(Cell), Cell >= 0, Cell =< 9 ->
    {ok, digit_atom($0 + Cell)};
normalize_cell(_) ->
    {error, invalid_cell}.

-spec digit_atom($0 | $1 | $2 | $3 | $4 | $5 | $6 | $7 | $8 | $9) -> grid_state().
digit_atom($0) -> '0';
digit_atom($1) -> '1';
digit_atom($2) -> '2';
digit_atom($3) -> '3';
digit_atom($4) -> '4';
digit_atom($5) -> '5';
digit_atom($6) -> '6';
digit_atom($7) -> '7';
digit_atom($8) -> '8';
digit_atom($9) -> '9'.
