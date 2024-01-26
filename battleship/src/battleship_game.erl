-module(battleship_game).
-export([
    place_fleet_random/0, 
    init_mock_game/0, 
    get_opposite_player/2, 
    get_player_by_id/2, 
    next_move/3
]).
-include("battleship.hrl").


-spec place_fleet_random() -> board().
place_fleet_random() ->
    try place_ships(battleship_board:init_grid(), battleship_ship:fleet()) 
    catch
        _:_ -> place_fleet_random()
    end.

-spec init_mock_game() -> #game{}.
init_mock_game() ->
    Player1 = #player{id = 1, board = place_fleet_random()},
    Player2 = #player{id = 2, board = place_fleet_random()},
    #game{
        player_one = Player1, 
        player_two = Player2, 
        first_turn = battleship_utils:get_random_binary(Player1, Player2),
        turns = []
    }.


-spec get_player_by_id(#game{}, player_id()) -> #player{}.
get_player_by_id(Game, Id) ->
    case Id =:= Game#game.player_one#player.id of
        true -> Game#game.player_two;
        false -> Game#game.player_one
    end.    

-spec get_opposite_player(#game{}, #player{}) -> #game{}.
get_opposite_player(Game, Player) ->
    case Player#player.id =:= Game#game.player_one#player.id of
        true -> Game#game.player_two;
        false -> Game#game.player_one
    end.       
 
-spec next_move(#game{}, row(), column()) -> #game{}.
next_move(Game, Row, Column) ->
    % if the game has no turns the first move is for first turn player
    CurrentPlayer = case Game#game.turns of
        [] -> Game#game.first_turn;
        [H|_] -> get_player_by_id(Game, H#strike.id)     
    end,
    OppositePlayer = get_opposite_player(Game, CurrentPlayer),
    Board = OppositePlayer#player.board,
    case strike(Board, Row, Column) of
        {'MISS', NewBoard} ->
            battleship_board:update_board(Game, OppositePlayer, NewBoard);
        {'ERROR', _} -> error("Wrong move");    
        {HitVal, NewBoard} -> 
            case battleship_board:count(NewBoard, ?HIT) == battleship_ship:fleet_size() of
                true -> "Game over";
                false -> 
                    HitCount = battleship_board:count(NewBoard, HitVal),
                    BlockedBoard = case HitVal of
                        '9' -> battleship_board:set_adjacents_blocked(NewBoard);
                        '8' -> battleship_board:set_adjacents_blocked(NewBoard);
                        '7' -> battleship_board:set_adjacents_blocked(NewBoard);
                        _ when HitCount =:= 0 -> battleship_board:set_adjacents_blocked(NewBoard);
                        _ -> NewBoard
                    end,
                    battleship_board:update_board(Game, OppositePlayer, BlockedBoard) 
            end
    end.

%%% ---------------------------------------------------
%%% Private functions.
%%% ---------------------------------------------------

strike(Grid, Row, Column) ->
    case battleship_board:get_cell_value(Grid, Row, Column) of
        ?EMPTY -> {'MISS', battleship_board:update_cell_at(Grid, Row, Column, ?MISS)};
        ?BLOCKED -> {'ERROR', Grid};
        ?HIT -> {'ERROR', Grid};
        ?MISS -> {'ERROR', Grid};
        _ -> {battleship_board:get_cell_value(Grid, Row, Column), battleship_board:update_cell_at(Grid, Row, Column, ?HIT)}
    end.

try_place_ship_random(_, _, 0) -> throw("Unable to place ship");
try_place_ship_random(Grid, Ship, Count) ->
    {Column, Row, Orientation} = get_random_ship_coordinate(),
    RandomShip = Ship#ship{row = Row, column = Column, orientation = Orientation},    
    Legal = battleship_board:is_legal(Grid, RandomShip),                    
    case Legal of
        true -> battleship_board:attach_ship(Grid, RandomShip);
        false -> try_place_ship_random(Grid, Ship, Count-1)
    end.


place_ships(Grid, []) -> Grid;
place_ships(Grid, [H|T]) ->
    NewGrid = try_place_ship_random(Grid, H, 100), 
    place_ships(NewGrid, T).

get_random_ship_coordinate() -> {rand:uniform(10), rand:uniform(10), battleship_utils:get_random_binary('VERTICAL', 'HORIZONTAL')}.
