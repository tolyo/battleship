-type ship_state() :: 'ACTIVE' | 'DAMAGED' | 'KILLED'.
-define(EMPTY, '_').
-define(BLOCKED, 'o').
-define(HIT, '+').
-define(MISS, 'x').
-type grid_state() ::
    ?EMPTY | ?BLOCKED | ?HIT | ?MISS | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'.
-type strike_res() :: 'MISS' | 'HIT' | 'ERROR'.
-type ship_orientation() :: 'VERTICAL' | 'HORIZONTAL'.
-type row() :: pos_integer().
-type column() :: pos_integer().
-type player_id() :: binary().
-type board() :: [[atom()]].
-record(ship, {
    id :: atom(),
    health :: ship_state(),
    orientation :: ship_orientation(),
    column :: non_neg_integer(),
    row :: non_neg_integer(),
    hitcount :: non_neg_integer(),
    size :: non_neg_integer()
}).
-type fleet() :: [#ship{}].
-record(strike, {
    id :: player_id(),
    x :: pos_integer(),
    y :: pos_integer(),
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

-record(user, {
    % UUID as text
    id :: binary(),
    username :: binary(),
    email :: binary(),
    password_hash :: binary(),
    rating :: integer(),
    created_at :: calendar:datetime()
}).

-type user() :: #user{}.
