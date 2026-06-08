-module(battleship_unit).
-export([all/0, create/2, total_size/0]).
-include_lib("battleship/include/rules/battleship.hrl").

-spec create(atom(), non_neg_integer()) -> #unit{}.
%% @doc Create one default Battleship unit with an id and size.
%% Sample usage: `Cruiser = battleship_unit:create('1', 3).`
create(Id, Size) ->
    #unit{
        id = Id,
        size = Size,
        health = 'ACTIVE',
        hitcount = 0,
        column = 0,
        row = 2,
        orientation = 'HORIZONTAL'
    }.

-spec all() -> unit_collection().
%% @doc Return the full default Battleship unit collection.
%% Sample usage: `Units = battleship_unit:all().`
all() ->
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

-spec total_size() -> non_neg_integer().
%% @doc Return the total occupied-cell count for the default unit collection.
%% Sample usage: `WinAt = battleship_unit:total_size().`
total_size() -> total_size(all()).

total_size([]) ->
    0;
total_size([Unit | Units]) ->
    Unit#unit.size + total_size(Units).
