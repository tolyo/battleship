-module(room_test).

-include_lib("eunit/include/eunit.hrl").

client_loop(Parent) ->
    receive
        {socket_send, Payload} ->
            Parent ! {socket_send, self(), Payload},
            client_loop(Parent);
        stop ->
            ok
    end.

room_move_flow_test() ->
    Parent = self(),
    Pid1 = spawn(fun() -> client_loop(Parent) end),
    Pid2 = spawn(fun() -> client_loop(Parent) end),
    Board = battleship_board:init_board(),
    Player1 = #{pid => Pid1, id => <<"p1">>, name => <<"p1">>, board => Board},
    Player2 = #{pid => Pid2, id => <<"p2">>, name => <<"p2">>, board => Board},
    {ok, RoomPid} = room:start_link(<<"room-test">>, battleship_rules, [Player1, Player2], #{}),
    unlink(RoomPid),
    try
        receive
            {socket_send, Pid1, #{
                type := <<"room_state">>,
                view := #{phase := <<"playing">>, allowed_actions := _}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,
        receive
            {socket_send, Pid2, #{
                type := <<"room_state">>,
                view := #{phase := <<"playing">>, allowed_actions := _}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,

        {ok, Snapshot} = gen_statem:call(RoomPid, state),
        First = maps:get(first_turn, Snapshot),
        Opponent =
            case First of
                <<"p1">> -> <<"p2">>;
                _ -> <<"p1">>
            end,

        ?assertMatch(
            {error, <<"action_not_allowed">>},
            gen_statem:call(RoomPid, {action, Opponent, move_action(0, 0)})
        ),
        ?assertEqual(ok, gen_statem:call(RoomPid, {action, First, move_action(0, 0)})),

        receive
            {socket_send, _Pid, #{
                type := <<"room_update">>,
                view := #{phase := <<"playing">>, allowed_actions := _}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,

        {ok, Updated} = gen_statem:call(RoomPid, state),
        [Strike] = maps:get(turns, Updated),
        ?assertEqual(First, maps:get(id, Strike))
    after
        Pid1 ! stop,
        Pid2 ! stop,
        exit(RoomPid, shutdown)
    end.

room_hit_keeps_player_turn_test() ->
    Parent = self(),
    Pid1 = spawn(fun() -> client_loop(Parent) end),
    Pid2 = spawn(fun() -> client_loop(Parent) end),
    EmptyBoard = battleship_board:init_board(),
    TargetBoard0 = battleship_board:update_cell_at(EmptyBoard, 1, 1, '0'),
    TargetBoard = battleship_board:update_cell_at(TargetBoard0, 1, 2, '0'),
    Player1 = #{pid => Pid1, id => <<"p1">>, name => <<"p1">>, board => TargetBoard},
    Player2 = #{pid => Pid2, id => <<"p2">>, name => <<"p2">>, board => TargetBoard},
    {ok, RoomPid} = room:start_link(<<"room-hit-test">>, battleship_rules, [Player1, Player2], #{}),
    unlink(RoomPid),
    try
        receive
            {socket_send, Pid1, #{
                type := <<"room_state">>,
                view := #{phase := <<"playing">>, allowed_actions := _}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,
        receive
            {socket_send, Pid2, #{
                type := <<"room_state">>,
                view := #{phase := <<"playing">>, allowed_actions := _}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,

        {ok, Snapshot} = gen_statem:call(RoomPid, state),
        First = maps:get(first_turn, Snapshot),
        Opponent =
            case First of
                <<"p1">> -> <<"p2">>;
                _ -> <<"p1">>
            end,

        ?assertEqual(ok, gen_statem:call(RoomPid, {action, First, move_action(0, 0)})),
        receive
            {socket_send, _Pid, #{
                type := <<"room_update">>,
                view := #{phase := <<"playing">>, allowed_actions := _}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,
        ?assertMatch(
            {error, <<"action_not_allowed">>},
            gen_statem:call(RoomPid, {action, Opponent, move_action(0, 0)})
        ),
        ?assertEqual(ok, gen_statem:call(RoomPid, {action, First, move_action(0, 1)})),

        {ok, Updated} = gen_statem:call(RoomPid, state),
        [SecondStrike, FirstStrike | _] = maps:get(turns, Updated),
        ?assertEqual(First, maps:get(id, FirstStrike)),
        ?assertEqual('HIT', maps:get(res, FirstStrike)),
        ?assertEqual(First, maps:get(id, SecondStrike)),
        ?assertEqual('HIT', maps:get(res, SecondStrike))
    after
        Pid1 ! stop,
        Pid2 ! stop,
        exit(RoomPid, shutdown)
    end.

room_rejects_move_on_blocked_cell_test() ->
    Parent = self(),
    Pid1 = spawn(fun() -> client_loop(Parent) end),
    Pid2 = spawn(fun() -> client_loop(Parent) end),
    EmptyBoard = battleship_board:init_board(),
    TargetBoard = battleship_board:update_cell_at(EmptyBoard, 2, 2, '9'),
    Player1 = #{pid => Pid1, id => <<"p1">>, name => <<"p1">>, board => TargetBoard},
    Player2 = #{pid => Pid2, id => <<"p2">>, name => <<"p2">>, board => TargetBoard},
    {ok, RoomPid} = room:start_link(
        <<"room-blocked-test">>, battleship_rules, [Player1, Player2], #{}
    ),
    unlink(RoomPid),
    try
        receive
            {socket_send, Pid1, #{
                type := <<"room_state">>,
                view := #{phase := <<"playing">>, allowed_actions := _}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,
        receive
            {socket_send, Pid2, #{
                type := <<"room_state">>,
                view := #{phase := <<"playing">>, allowed_actions := _}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,

        {ok, Snapshot} = gen_statem:call(RoomPid, state),
        First = maps:get(first_turn, Snapshot),

        ?assertEqual(ok, gen_statem:call(RoomPid, {action, First, move_action(1, 1)})),
        ?assertMatch(
            {error, <<"invalid_move">>},
            gen_statem:call(RoomPid, {action, First, move_action(0, 0)})
        ),

        {ok, Updated} = gen_statem:call(RoomPid, state),
        ?assertEqual(1, length(maps:get(turns, Updated)))
    after
        Pid1 ! stop,
        Pid2 ! stop,
        exit(RoomPid, shutdown)
    end.

move_action(Row, Column) ->
    #{type => <<"move">>, row => Row, column => Column}.
