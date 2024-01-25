-type ship_state() :: 'ACTIVE' | 'DAMAGED' | 'KILLED'.

-type grid_state() :: 'EMPTY' | 'FILLED' | 'BLOCKED' | 'HIT' | 'MISS'.

-type ship_orientation() :: 'VERTICAL' | 'HORIZONTAL'.

-record(ship, {
    id :: string,
    health :: ship_state(),
    orientation :: ship_orientation(),
    column :: number(),
    row :: number(),
    grid_state :: [number()],
    hitcount :: number(),
    size :: number()
}).

-record(fleet, {
    ships :: [ship]
}).

-record(board, {
    grid :: [[grid_state()]]
}).

-record(strike, {
    x :: number(),
    y :: number()
}).

-record(game, {
    player_one :: string,
    player_two :: string,
    player_one_board :: #board{},
    player_two_board :: #board{},
    first_turn :: string,
    turns :: [strike]
}).

% Map tile representation
-define(EMPTY, '_').
-define(FILLED, 'X').
-define(BLOCKED, 'o').
-define(HIT, '+').
-define(MISS, 'm').