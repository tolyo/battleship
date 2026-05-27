-module(battleship_ship).
-export([create/2, fleet/0, fleet_size/0]).
-include_lib("battleship/include/battleship.hrl").

-spec create(atom(), non_neg_integer()) -> #ship{}.
create(Id, Size) ->
    #ship{
        id = Id,
        size = Size,
        health = 'ACTIVE',
        hitcount = 0,
        column = 0,
        row = 2,
        orientation = 'HORIZONTAL'
    }.

-spec fleet() -> fleet().
fleet() ->
    [
        create('0', 4),
        create('1', 3),
        create('2', 3),
        create('3', 2),
        create('4', 2),
        create('5', 2),
        create('6', 1),
        create('7', 1),
        create('8', 1),
        create('9', 1)
    ].

-spec fleet_size() -> non_neg_integer().
fleet_size() -> fleet_size(fleet()).

fleet_size([]) ->
    0;
fleet_size([Ship | Ships]) ->
    Ship#ship.size + fleet_size(Ships).
