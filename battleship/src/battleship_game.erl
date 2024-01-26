-module(battleship_game).
-compile(export_all).
-include("battleship.hrl").

grid() -> lists:seq(1, 10).

init_grid() -> [[?EMPTY || _ <- grid()] || _ <- grid()].

get_random_tile() ->
    rand:uniform(10).

get_random_orientation() ->
    get_random_binary('VERTICAL', 'HORIZONTAL').

get_random_binary(Val1, Val2) ->
    case rand:uniform(2) of
        1 -> Val1;
        2 -> Val2
    end.    

get_random_ship_coordinate() ->
    { get_random_tile(), get_random_tile(), get_random_orientation()}.

get_adjacent_coordinates(Row, Column) ->
    [{R, C} || 
        R <- [Row-1, Row, Row+1], 
        C <- [Column-1, Column, Column+1], 
        R >= 1, 
        R =< 10, 
        C >= 1, 
        C =< 10, 
        {R, C} /= {Row, Column} 
    ].

is_legal(Grid, #ship{row=Row, column=Column, size=Size, orientation=Orientation}) ->
    case Orientation of
        'HORIZONTAL' -> is_legal_horizontal(Grid, Row, Column, Size);
        'VERTICAL' -> is_legal_vertical(Grid, Row, Column, Size)
    end.
is_legal_horizontal(Grid, Row, Column, Size) when Row >= 1, Row =< 10, Column >= 1, Column =< 10 -> 
    is_legal_horizontal(Grid, Row, Column, Size, true).
is_legal_horizontal(_Grid, _Row, _Column, 0, IsLegal) -> IsLegal;
is_legal_horizontal(Grid, Row, Column, Size, IsLegal) when Row >= 1, Row =< 10, Column >= 1, Column =< 10 ->
    case {is_cell_empty(Grid, Row, Column), is_adjacent_cells_empty(Grid, Row, Column)} of
        {true, true} -> is_legal_horizontal(Grid, Row, Column + 1, Size - 1, IsLegal);
        _ -> false
    end;
is_legal_horizontal(_, _, _, _, _) -> false.

is_legal_vertical(Grid, Row, Column, Size) when Row >= 1, Row =< 10, Column >= 1, Column =< 10 ->
    is_legal_vertical(Grid, Row, Column, Size, true).
is_legal_vertical(_Grid, _Row, _Column, 0, IsLegal) -> IsLegal;
is_legal_vertical(Grid, Row, Column, Size, IsLegal) when Row >= 0, Row =< 9, Column >= 0, Column =< 9 ->
    case {is_cell_empty(Grid, Row, Column), is_adjacent_cells_empty(Grid, Row, Column)} of
        {true, true} -> is_legal_vertical(Grid, Row + 1, Column, Size - 1, IsLegal);
        _ -> false
    end;
is_legal_vertical(_, _, _, _, _) -> false.

is_cell_empty(Grid, Row, Column) ->
    get_cell_value(Grid, Row, Column) =:= ?EMPTY.
get_cell_value(Grid, Row, Column) ->
    lists:nth(Column, lists:nth(Row, Grid)).
update_cell_at(Grid, Row, Column, Value) ->
    update_list_at(Grid, Row, update_list_at(lists:nth(Row, Grid), Column, Value)).

is_adjacent_cells_empty(Grid, Row, Column) ->
    lists:all(fun({R, C}) -> is_cell_empty(Grid, R, C) end, get_adjacent_coordinates(Row, Column)).

attach_ship(Grid, #ship{id = Id, row = Row, column = Column, size = Size, orientation=Orientation}) ->
    case Orientation of
        'HORIZONTAL' -> attach_ship_horizontal(Grid, Id, Row, Column, Size);
        'VERTICAL' -> attach_ship_vertical(Grid, Id, Row, Column, Size)
    end.

attach_ship_horizontal(Grid, _, _, _, 0) -> Grid;
attach_ship_horizontal(Grid, Id, Row, Column, Size) ->
    NewRow = update_list_at(lists:nth(Row, Grid), Column, Id),
    NewGrid = update_list_at(Grid, Row, NewRow),
    attach_ship_horizontal(NewGrid, Id, Row, Column + 1, Size - 1).

attach_ship_vertical(Grid, _, _, _, 0) -> Grid;
attach_ship_vertical(Grid, Id, Row, Column, Size) ->
    NewRow = update_list_at(lists:nth(Row, Grid), Column, Id),
    NewGrid = update_list_at(Grid, Row, NewRow),
    attach_ship_vertical(NewGrid, Id, Row + 1, Column, Size - 1).

update_list_at([_|T], 1, V) -> [V | T];
update_list_at([H|T], I, V) -> [H | update_list_at(T, I - 1, V)].

try_place_ship_random(_, _, 0) -> throw("Unable to place ship");
try_place_ship_random(Grid, Ship, Count) ->
    {Column, Row, Orientation} = get_random_ship_coordinate(),
    RandomShip = Ship#ship{row = Row, column = Column, orientation = Orientation},    
    Legal = is_legal(Grid, RandomShip),                    
    case Legal of
        true -> attach_ship(Grid, RandomShip);
        false -> try_place_ship_random(Grid, Ship, Count-1)
    end.


place_ships(Grid, []) -> Grid;
place_ships(Grid, [H|T]) ->
    NewGrid = try_place_ship_random(Grid, H, 100), 
    place_ships(NewGrid, T).


place_fleet_random() ->
    try place_ships(init_grid(), battleship_ship:fleet()) 
    catch
        _:_ -> place_fleet_random()
    end.



init_mock_game() ->
    Player1 = #player{id = 1, board = place_fleet_random()},
    Player2 = #player{id = 2, board = place_fleet_random()},
    #game{
        player_one = Player1, 
        player_two = Player2, 
        first_turn = get_random_binary(Player1, Player2),
        turns = []
    }.

get_player_by_id(Game, Id) ->
    case Id =:= Game#game.player_one#player.id of
        true -> Game#game.player_two;
        false -> Game#game.player_one
    end.    

get_opposite_player(Game, Player) ->
    case Player#player.id =:= Game#game.player_one#player.id of
        true -> Game#game.player_two;
        false -> Game#game.player_one
    end.       


update_board(Game, Player, Board) ->     
    case Player#player.id =:= Game#game.player_one#player.id of
        true -> Game#game.player_one#player{board = Board};
        false -> Game#game.player_two#player{board = Board}
    end.

count(Grid, Val) ->
    length(lists:filter(fun(X) -> X =:= Val end, lists:flatten(Grid))).
    
strike(Grid, Row, Column) ->
    case get_cell_value(Grid, Row, Column) of
        ?EMPTY -> {'MISS', update_cell_at(Grid, Row, Column, ?MISS)};
        ?BLOCKED -> {'ERROR', Grid};
        ?HIT -> {'ERROR', Grid};
        ?MISS -> {'ERROR', Grid};
        _ -> {get_cell_value(Grid, Row, Column), update_cell_at(Grid, Row, Column, ?HIT)}
    end.

set_adjacents_blocked(Grid) ->
    Coords = [{R, C} || R <- grid(), C <- grid()],
    set_adjacents_blocked(Coords, Grid).

set_adjacents_blocked([], Grid) -> Grid;
set_adjacents_blocked([{Row, Column}|T], Grid) -> 
    case get_cell_value(Grid, Row, Column) of
        ?HIT -> set_adjacents_blocked(T, update_blocked(get_adjacent_coordinates(Row, Column), Grid));
        _ -> set_adjacents_blocked(T, Grid)
    end.    
update_blocked([], Grid) -> Grid;
update_blocked([{Row, Column}| T], Grid) -> 
    case get_cell_value(Grid, Row, Column) of
        ?EMPTY -> update_blocked(T, update_cell_at(Grid, Row, Column, ?BLOCKED));
        _ -> update_blocked(T, Grid)
    end.
next_move(Game, Row, Column) ->
    % if the game has no turns the first move is for first turn player
    CurrentPlayer = case Game#game.turns of
        [] -> Game#game.first_turn;
        [H|_] -> get_player_by_id(Game, H#strike.player_id)     
    end,
    OppositePlayer = get_opposite_player(Game, CurrentPlayer),
    Board = OppositePlayer#player.board,
    case strike(Board, Row, Column) of
        {'MISS', NewBoard} ->
            update_board(Game, OppositePlayer, NewBoard);
        {'ERROR', _} -> error("Wrong move");    
        {HitVal, NewBoard} -> 
            case count(NewBoard, ?HIT) == battleship_ship:fleet_size() of
                true -> "Game over";
                false -> 
                    HitCount = count(NewBoard, HitVal),
                    BlockedBoard = case HitVal of
                        '9' -> set_adjacents_blocked(NewBoard);
                        '8' -> set_adjacents_blocked(NewBoard);
                        '7' -> set_adjacents_blocked(NewBoard);
                        _ when HitCount =:= 0 -> set_adjacents_blocked(NewBoard)
                    end,
                    update_board(Game, OppositePlayer, BlockedBoard) 
            end
    end.


