-module(battleship_game).
-compile(export_all).
-include("battleship.hrl").

grid() -> lists:seq(1, 10).

init_grid() -> [[?EMPTY || _ <- grid()] || _ <- grid()].

get_random_tile() ->
    rand:uniform(10).

get_random_orientation() ->
    case rand:uniform(2) of
        1 -> 'VERTICAL';
        2 -> 'HORIZONTAL'
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
    lists:nth(Column, lists:nth(Row, Grid)) =:= ?EMPTY.

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