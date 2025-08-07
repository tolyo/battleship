-type ship_state() :: 'ACTIVE' | 'DAMAGED' | 'KILLED'.
-define(EMPTY, '_').
-define(BLOCKED, 'o').
-define(HIT, '+').
-define(MISS, 'x').
-type grid_state() ::
    ?EMPTY | ?BLOCKED | ?HIT | ?MISS | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'.
-type strike_res() :: 'MISS' | 'HIT' | 'ERROR'.
-type ship_orientation() :: 'VERTICAL' | 'HORIZONTAL'.
-type row() :: integer().
-type column() :: integer().
-type player_id() :: [char()].
-type board() :: [[grid_state()]].
-record(ship, {
    id :: atom(),
    health :: ship_state(),
    orientation :: ship_orientation(),
    column :: number(),
    row :: number(),
    hitcount :: number(),
    size :: number()
}).
-type fleet() :: [#ship{}].
-record(strike, {
    id :: player_id(),
    x :: number(),
    y :: number(),
    res :: strike_res()
}).
-record(player, {
    id :: player_id(),
    board :: board()
}).
-record(game, {
    player_one :: #player{},
    player_two :: #player{},
    first_turn :: player_id(),
    turns :: [#strike{}],
    state :: 'INIT' | 'ACTIVE' | 'FINISHED'
}).
