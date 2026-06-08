-module(battleship_board).
%% @doc Board helpers for placement and strike updates.
-export([
    init_board/0,
    is_legal/2,
    is_cell_empty/3,
    get_cell_value/3,
    update_cell_at/4,
    attach_unit/2,
    set_adjacents_blocked/1,
    count/2
]).
-include_lib("battleship/include/rules/battleship.hrl").

%% ------------------------------------------------------------------
%% Public API.
%% ------------------------------------------------------------------

-spec init_board() -> board().
%% @doc Create an empty 10x10 Battleship board.
%% Sample usage: `Board = battleship_board:init_board().`
init_board() -> [[?EMPTY || _ <- grid()] || _ <- grid()].

-spec is_legal(board(), #unit{}) -> boolean().
%% @doc Return whether a unit can be placed without overlap or adjacency conflicts.
%% Sample usage: `battleship_board:is_legal(Board, Unit#unit{row = 1, column = 1}).`
is_legal(Board, #unit{row = Row, column = Column, size = Size, orientation = Orientation}) ->
    case Orientation of
        'HORIZONTAL' -> is_legal_horizontal(Board, Row, Column, Size);
        'VERTICAL' -> is_legal_vertical(Board, Row, Column, Size)
    end.

-spec is_cell_empty(board(), row(), column()) -> boolean().
%% @doc Return whether a one-based board coordinate contains `?EMPTY`.
%% Sample usage: `battleship_board:is_cell_empty(Board, 1, 1).`
is_cell_empty(Board, Row, Column) ->
    get_cell_value(Board, Row, Column) =:= ?EMPTY.

-spec get_cell_value(board(), row(), column()) -> grid_state().
%% @doc Read a one-based board coordinate.
%% Sample usage: `Cell = battleship_board:get_cell_value(Board, 1, 1).`
get_cell_value(Board, Row, Column) ->
    lists:nth(Column, lists:nth(Row, Board)).

-spec update_cell_at(board(), row(), column(), grid_state()) -> board().
%% @doc Return a board with one one-based coordinate replaced.
%% Sample usage: `Next = battleship_board:update_cell_at(Board, 1, 1, ?MISS).`
update_cell_at(Board, Row, Column, Value) ->
    utils:update_list_at(
        Board,
        Row,
        utils:update_list_at(
            lists:nth(Row, Board), Column, Value
        )
    ).

-spec attach_unit(board(), #unit{}) -> board().
%% @doc Return a board with a unit's id written across its occupied cells.
%% Sample usage: `Next = battleship_board:attach_unit(Board, Unit).`
attach_unit(Board, #unit{
    id = Id, row = Row, column = Column, size = Size, orientation = Orientation
}) ->
    case Orientation of
        'HORIZONTAL' -> attach_unit_horizontal(Board, Id, Row, Column, Size);
        'VERTICAL' -> attach_unit_vertical(Board, Id, Row, Column, Size)
    end.

-spec set_adjacents_blocked(board()) -> board().
%% @doc Mark empty cells adjacent to hits as blocked.
%% Sample usage: `Next = battleship_board:set_adjacents_blocked(Board).`
set_adjacents_blocked(Board) ->
    Coords = [{R, C} || R <- grid(), C <- grid()],
    set_adjacents_blocked(Coords, Board).

-spec count(board(), grid_state()) -> non_neg_integer().
%% @doc Count cells with a given value.
%% Sample usage: `Hits = battleship_board:count(Board, ?HIT).`
count(Board, Value) ->
    count_rows(Board, Value).

%% ------------------------------------------------------------------
%% Private helpers.
%% ------------------------------------------------------------------

grid() -> lists:seq(1, 10).

count_rows([], _Value) ->
    0;
count_rows([Row | Rows], Value) ->
    count_row(Row, Value) + count_rows(Rows, Value).

count_row([], _Value) ->
    0;
count_row([Value | Cells], Value) ->
    1 + count_row(Cells, Value);
count_row([_Cell | Cells], Value) ->
    count_row(Cells, Value).

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

attach_unit_horizontal(Board, _, _, _, 0) ->
    Board;
attach_unit_horizontal(Board, Id, Row, Column, Size) ->
    NewRow = utils:update_list_at(lists:nth(Row, Board), Column, Id),
    NewBoard = utils:update_list_at(Board, Row, NewRow),
    attach_unit_horizontal(NewBoard, Id, Row, Column + 1, Size - 1).

attach_unit_vertical(Board, _, _, _, 0) ->
    Board;
attach_unit_vertical(Board, Id, Row, Column, Size) ->
    NewRow = utils:update_list_at(lists:nth(Row, Board), Column, Id),
    NewBoard = utils:update_list_at(Board, Row, NewRow),
    attach_unit_vertical(NewBoard, Id, Row + 1, Column, Size - 1).

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
