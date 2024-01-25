-module(battleship_ship).
-compile(export_all).
-include("battleship.hrl").


create(Id, Size) ->
    #ship{
        id = Id, 
        size = Size,
        health = 'ACTIVE',
        grid_state = lists:duplicate(Size, true),
        hitcount = 0,
        orientation = 'HORIZONTAL'
    }.

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

fleet_size() ->
    lists:sum(
        lists:map(
            fun(#ship{size = Size}) -> Size end, 
            fleet()
        )
    ).    