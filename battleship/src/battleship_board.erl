-module(battleship_board).
-export([
    init_board/0,
    is_legal/2,
    is_cell_empty/3,
    get_cell_value/3,
    update_cell_at/4,
    attach_ship/2,
    set_adjacents_blocked/1
]).
-include("battleship.hrl").

-spec init_board() -> board().
init_board() -> [[?EMPTY || _ <- grid()] || _ <- grid()].

-spec is_legal(board(), #ship{}) -> boolean().
is_legal(Board, #ship{row = Row, column = Column, size = Size, orientation = Orientation}) ->
    case Orientation of
        'HORIZONTAL' -> is_legal_horizontal(Board, Row, Column, Size);
        'VERTICAL' -> is_legal_vertical(Board, Row, Column, Size)
    end.

-spec is_cell_empty(board(), row(), column()) -> boolean().
is_cell_empty(Board, Row, Column) ->
    get_cell_value(Board, Row, Column) =:= ?EMPTY.

-spec get_cell_value(board(), row(), column()) -> boolean().
get_cell_value(Board, Row, Column) ->
    lists:nth(Column, lists:nth(Row, Board)).

-spec update_cell_at(board(), row(), column(), any()) -> board().
update_cell_at(Board, Row, Column, Value) ->
    battleship_utils:update_list_at(
        Board,
        Row,
        battleship_utils:update_list_at(
            lists:nth(Row, Board), Column, Value
        )
    ).

-spec attach_ship(board(), #ship{}) -> board().
attach_ship(Board, #ship{
    id = Id, row = Row, column = Column, size = Size, orientation = Orientation
}) ->
    case Orientation of
        'HORIZONTAL' -> attach_ship_horizontal(Board, Id, Row, Column, Size);
        'VERTICAL' -> attach_ship_vertical(Board, Id, Row, Column, Size)
    end.

-spec set_adjacents_blocked(board()) -> board().
set_adjacents_blocked(Board) ->
    Coords = [{R, C} || R <- grid(), C <- grid()],
    set_adjacents_blocked(Coords, Board).

%%% ---------------------------------------------------
%%% Private functions.
%%% ---------------------------------------------------

grid() -> lists:seq(1, 10).

is_adjacent_cells_empty(Board, Row, Column) ->
    lists:all(fun({R, C}) -> is_cell_empty(Board, R, C) end, get_adjacent_coordinates(Row, Column)).
get_adjacent_coordinates(Row, Column) ->
    [
        {R, C}
     || R <- [Row - 1, Row, Row + 1],
        C <- [Column - 1, Column, Column + 1],
        R >= 1,
        R =< 10,
        C >= 1,
        C =< 10,
        {R, C} /= {Row, Column}
    ].

attach_ship_horizontal(Board, _, _, _, 0) ->
    Board;
attach_ship_horizontal(Board, Id, Row, Column, Size) ->
    NewRow = battleship_utils:update_list_at(lists:nth(Row, Board), Column, Id),
    NewBoard = battleship_utils:update_list_at(Board, Row, NewRow),
    attach_ship_horizontal(NewBoard, Id, Row, Column + 1, Size - 1).

attach_ship_vertical(Board, _, _, _, 0) ->
    Board;
attach_ship_vertical(Board, Id, Row, Column, Size) ->
    NewRow = battleship_utils:update_list_at(lists:nth(Row, Board), Column, Id),
    NewBoard = battleship_utils:update_list_at(Board, Row, NewRow),
    attach_ship_vertical(NewBoard, Id, Row + 1, Column, Size - 1).

set_adjacents_blocked([], Board) ->
    Board;
set_adjacents_blocked([{Row, Column} | T], Board) ->
    case get_cell_value(Board, Row, Column) of
        ?HIT ->
            set_adjacents_blocked(T, update_blocked(get_adjacent_coordinates(Row, Column), Board));
        _ ->
            set_adjacents_blocked(T, Board)
    end.
update_blocked([], Board) ->
    Board;
update_blocked([{Row, Column} | T], Board) ->
    case get_cell_value(Board, Row, Column) of
        ?EMPTY -> update_blocked(T, update_cell_at(Board, Row, Column, ?BLOCKED));
        _ -> update_blocked(T, Board)
    end.

is_legal_horizontal(Board, Row, Column, Size) when Row >= 1, Row =< 10, Column >= 1, Column =< 10 ->
    is_legal_horizontal(Board, Row, Column, Size, true).

is_legal_horizontal(_Board, _Row, _Column, 0, IsLegal) ->
    IsLegal;
is_legal_horizontal(Board, Row, Column, Size, IsLegal) when
    Row >= 1, Row =< 10, Column >= 1, Column =< 10
->
    case {is_cell_empty(Board, Row, Column), is_adjacent_cells_empty(Board, Row, Column)} of
        {true, true} -> is_legal_horizontal(Board, Row, Column + 1, Size - 1, IsLegal);
        _ -> false
    end;
is_legal_horizontal(_, _, _, _, _) ->
    false.

is_legal_vertical(Board, Row, Column, Size) when Row >= 1, Row =< 10, Column >= 1, Column =< 10 ->
    is_legal_vertical(Board, Row, Column, Size, true).
is_legal_vertical(_Board, _Row, _Column, 0, IsLegal) ->
    IsLegal;
is_legal_vertical(Board, Row, Column, Size, IsLegal) when
    Row >= 0, Row =< 9, Column >= 0, Column =< 9
->
    case {is_cell_empty(Board, Row, Column), is_adjacent_cells_empty(Board, Row, Column)} of
        {true, true} -> is_legal_vertical(Board, Row + 1, Column, Size - 1, IsLegal);
        _ -> false
    end;
is_legal_vertical(_, _, _, _, _) ->
    false.
