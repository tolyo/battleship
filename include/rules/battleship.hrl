%% @doc Lifecycle state for a placed Battleship unit.
%% Sample usage: `#unit{health = 'ACTIVE'}`.
-type unit_state() :: 'ACTIVE' | 'DAMAGED' | 'KILLED'.
%% @doc Empty board cell marker.
%% Sample usage: `Cell = ?EMPTY`.
-define(EMPTY, '_').
%% @doc Board cell marker that cannot be targeted because it is adjacent to destroyed units.
%% Sample usage: `battleship_board:update_cell_at(Board, 1, 1, ?BLOCKED)`.
-define(BLOCKED, 'o').
%% @doc Board cell marker for a successful strike.
%% Sample usage: `battleship_board:count(Board, ?HIT)`.
-define(HIT, '+').
%% @doc Board cell marker for a missed strike.
%% Sample usage: `battleship_board:update_cell_at(Board, 1, 1, ?MISS)`.
-define(MISS, 'x').
%% @doc Any legal cell value in a Battleship board.
%% Sample usage: `-spec get_cell_value(board(), row(), column()) -> grid_state().`
-type grid_state() ::
    ?EMPTY | ?BLOCKED | ?HIT | ?MISS | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'.
%% @doc Result recorded for a strike action.
%% Sample usage: `#strike{res = 'HIT'}`.
-type strike_res() :: 'MISS' | 'HIT' | 'ERROR'.
%% @doc Orientation of a Battleship unit on the grid.
%% Sample usage: `#unit{orientation = 'HORIZONTAL'}`.
-type unit_orientation() :: 'VERTICAL' | 'HORIZONTAL'.
%% @doc One-based board row index.
%% Sample usage: `battleship_board:get_cell_value(Board, 1, 1)`.
-type row() :: pos_integer().
%% @doc One-based board column index.
%% Sample usage: `battleship_board:get_cell_value(Board, 1, 1)`.
-type column() :: pos_integer().
%% @doc Stable player identifier used inside Battleship records.
%% Sample usage: `#player{id = <<"p1">>}`.
-type player_id() :: binary().
%% @doc One serialized board cell.
%% Sample usage: `-type board() :: [[board_cell()]].`
-type board_cell() :: grid_state().
%% @doc Ten-by-ten Battleship board represented as rows of cells.
%% Sample usage: `Board = battleship_board:init_board().`
-type board() :: [[board_cell()]].
%% @doc Battleship unit definition and placement state.
%% Sample usage: `battleship_unit:create('0', 4)`.
-record(unit, {
    id :: atom(),
    health :: unit_state(),
    orientation :: unit_orientation(),
    column :: non_neg_integer(),
    row :: non_neg_integer(),
    hitcount :: non_neg_integer(),
    size :: non_neg_integer()
}).
%% @doc Collection of Battleship units to place on a board.
%% Sample usage: `Units = battleship_unit:all().`
-type unit_collection() :: [#unit{}].
%% @doc One completed strike in the turn history.
%% Sample usage: `#strike{id = PlayerId, x = 1, y = 1, res = 'MISS'}`.
-record(strike, {
    id :: player_id(),
    x :: pos_integer(),
    y :: pos_integer(),
    res :: strike_res()
}).
%% @doc Battleship participant state.
%% Sample usage: `#player{id = <<"p1">>, board = Board}`.
-record(player, {
    id :: player_id(),
    board :: board()
}).
%% @doc Battleship rules model stored inside the generic room state machine.
%% Sample usage: `#match{player_one = P1, player_two = P2, state = 'ACTIVE'}`.
-record(match, {
    player_one :: #player{},
    player_two :: #player{},
    first_turn :: player_id(),
    turns :: [#strike{}],
    state :: 'INIT' | 'ACTIVE' | 'FINISHED'
}).
