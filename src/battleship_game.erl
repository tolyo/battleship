-module(battleship_game).
%% @doc Core game state and move evaluation logic.
-export([
    place_fleet_random/0,
    init_mock_game/0,
    get_opposite_player/2,
    get_player_by_id/2,
    next_move/3
]).
-include_lib("battleship/include/battleship.hrl").

%% ------------------------------------------------------------------
%% Public API.
%% ------------------------------------------------------------------

-spec place_fleet_random() -> board().
place_fleet_random() ->
    try
        place_ships(battleship_board:init_board(), battleship_ship:fleet())
    catch
        _:_ -> place_fleet_random()
    end.

-spec init_mock_game() -> #game{}.
init_mock_game() ->
    Player1 = #player{id = <<"1">>, board = place_fleet_random()},
    Player2 = #player{id = <<"2">>, board = place_fleet_random()},
    #game{
        player_one = Player1,
        player_two = Player2,
        first_turn = battleship_utils:get_random_binary(Player1#player.id, Player2#player.id),
        turns = [],
        state = 'ACTIVE'
    }.

-spec get_player_by_id(#game{}, player_id()) -> #player{}.
get_player_by_id(Game, Id) ->
    case Id =:= Game#game.player_one#player.id of
        true -> Game#game.player_one;
        false -> Game#game.player_two
    end.

-spec get_opposite_player(#game{}, #player{}) -> #player{}.
get_opposite_player(Game, Player) ->
    case Player#player.id =:= Game#game.player_one#player.id of
        true -> Game#game.player_two;
        false -> Game#game.player_one
    end.

-spec next_move(#game{}, row(), column()) -> #game{}.
next_move(Game, Row, Column) ->
    % if the game has no turns the first move is for first turn player
    CurrentPlayer =
        case Game#game.turns of
            [] -> get_player_by_id(Game, Game#game.first_turn);
            [H | _] -> get_opposite_player(Game, get_player_by_id(Game, H#strike.id))
        end,
    OppositePlayer = get_opposite_player(Game, CurrentPlayer),
    Board = OppositePlayer#player.board,
    case strike(Board, Row, Column) of
        {'MISS', NewBoard} ->
            update_game(Game, CurrentPlayer, OppositePlayer, NewBoard, 'MISS', Row, Column);
        {'ERROR', _} ->
            error("Wrong move");
        {HitVal, NewBoard} ->
            case battleship_board:count(NewBoard, ?HIT) == battleship_ship:fleet_size() of
                true ->
                    update_game(
                        Game#game{state = 'FINISHED'},
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
                    update_game(
                        Game, CurrentPlayer, OppositePlayer, BlockedBoard, 'HIT', Row, Column
                    )
            end
    end.

%%% ---------------------------------------------------
%%% Private functions.
%%% ---------------------------------------------------

-type strike_value() :: strike_res() | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'.

-spec strike(board(), row(), column()) -> {strike_value(), board()}.
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

-spec update_game(#game{}, #player{}, #player{}, board(), strike_res(), row(), column()) -> #game{}.
update_game(Game, CurrentPlayer, OppositePlayer, NewBoard, Result, Row, Column) ->
    Strike = #strike{id = CurrentPlayer#player.id, x = Column, y = Row, res = Result},
    UpdatedGame = Game#game{turns = [Strike | Game#game.turns]},
    update_player_board(UpdatedGame, OppositePlayer, NewBoard).

-spec update_player_board(#game{}, #player{}, board()) -> #game{}.
update_player_board(Game, Player, Board) ->
    case Player#player.id =:= Game#game.player_one#player.id of
        true ->
            Game#game{player_one = Player#player{board = Board}};
        false ->
            Game#game{player_two = Player#player{board = Board}}
    end.

-spec try_place_ship_random(board(), #ship{}, non_neg_integer()) -> board().
try_place_ship_random(_, _, 0) ->
    throw("Unable to place ship");
try_place_ship_random(Board, Ship, Count) ->
    {Column, Row, Orientation} = get_random_ship_coordinate(),
    RandomShip = Ship#ship{row = Row, column = Column, orientation = Orientation},
    Legal = battleship_board:is_legal(Board, RandomShip),
    case Legal of
        true -> battleship_board:attach_ship(Board, RandomShip);
        false -> try_place_ship_random(Board, Ship, Count - 1)
    end.

-spec place_ships(board(), fleet()) -> board().
place_ships(Board, []) ->
    Board;
place_ships(Board, [H | T]) ->
    NewBoard = try_place_ship_random(Board, H, 100),
    place_ships(NewBoard, T).

-spec get_random_ship_coordinate() -> {column(), row(), ship_orientation()}.
get_random_ship_coordinate() ->
    {
        rand:uniform(10),
        rand:uniform(10),
        battleship_utils:get_random_binary('VERTICAL', 'HORIZONTAL')
    }.
