-module(battleship_room_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("battleship/include/battleship.hrl").

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
    {ok, RoomPid} = battleship_room:start_link(<<"room-test">>, Player1, Player2),
    unlink(RoomPid),
    try
        receive
            {socket_send, Pid1, #{
                type := <<"game_state">>,
                game := #{phase := <<"playing">>, current_turn := InitialTurn, winner := null}
            }} when is_binary(InitialTurn) ->
                ok
        after 1000 ->
            ?assert(false)
        end,
        receive
            {socket_send, Pid2, #{
                type := <<"game_state">>,
                game := #{phase := <<"playing">>, current_turn := InitialTurn2, winner := null}
            }} when is_binary(InitialTurn2) ->
                ok
        after 1000 ->
            ?assert(false)
        end,

        {ok, Game} = gen_server:call(RoomPid, state),
        First = Game#game.first_turn,
        Opponent =
            case First of
                <<"p1">> -> <<"p2">>;
                _ -> <<"p1">>
            end,

        ?assertMatch(
            {error, <<"not_your_turn">>},
            gen_server:call(RoomPid, {move, Opponent, 0, 0})
        ),
        ?assertEqual(ok, gen_server:call(RoomPid, {move, First, 0, 0})),

        receive
            {socket_send, _Pid, #{
                type := <<"game_update">>,
                game := #{phase := <<"playing">>, current_turn := Opponent, winner := null}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,

        {ok, Updated} = gen_server:call(RoomPid, state),
        ?assertEqual(1, length(Updated#game.turns)),
        Strike = hd(Updated#game.turns),
        ?assertEqual(First, Strike#strike.id)
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
    {ok, RoomPid} = battleship_room:start_link(<<"room-hit-test">>, Player1, Player2),
    unlink(RoomPid),
    try
        receive
            {socket_send, Pid1, #{
                type := <<"game_state">>,
                game := #{phase := <<"playing">>, current_turn := HitInitialTurn, winner := null}
            }} when is_binary(HitInitialTurn) ->
                ok
        after 1000 ->
            ?assert(false)
        end,
        receive
            {socket_send, Pid2, #{
                type := <<"game_state">>,
                game := #{phase := <<"playing">>, current_turn := HitInitialTurn2, winner := null}
            }} when is_binary(HitInitialTurn2) ->
                ok
        after 1000 ->
            ?assert(false)
        end,

        {ok, Game} = gen_server:call(RoomPid, state),
        First = Game#game.first_turn,
        Opponent =
            case First of
                <<"p1">> -> <<"p2">>;
                _ -> <<"p1">>
            end,

        ?assertEqual(ok, gen_server:call(RoomPid, {move, First, 0, 0})),
        receive
            {socket_send, _Pid, #{
                type := <<"game_update">>,
                game := #{phase := <<"playing">>, current_turn := First, winner := null}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,
        ?assertMatch(
            {error, <<"not_your_turn">>},
            gen_server:call(RoomPid, {move, Opponent, 0, 0})
        ),
        ?assertEqual(ok, gen_server:call(RoomPid, {move, First, 0, 1})),

        {ok, Updated} = gen_server:call(RoomPid, state),
        [SecondStrike, FirstStrike | _] = Updated#game.turns,
        ?assertEqual(First, FirstStrike#strike.id),
        ?assertEqual('HIT', FirstStrike#strike.res),
        ?assertEqual(First, SecondStrike#strike.id),
        ?assertEqual('HIT', SecondStrike#strike.res)
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
    {ok, RoomPid} = battleship_room:start_link(<<"room-blocked-test">>, Player1, Player2),
    unlink(RoomPid),
    try
        receive
            {socket_send, Pid1, #{
                type := <<"game_state">>,
                game := #{phase := <<"playing">>, current_turn := BlockedInitialTurn}
            }} when is_binary(BlockedInitialTurn) ->
                ok
        after 1000 ->
            ?assert(false)
        end,
        receive
            {socket_send, Pid2, #{
                type := <<"game_state">>,
                game := #{phase := <<"playing">>, current_turn := BlockedInitialTurn2}
            }} when is_binary(BlockedInitialTurn2) ->
                ok
        after 1000 ->
            ?assert(false)
        end,

        {ok, Game} = gen_server:call(RoomPid, state),
        First = Game#game.first_turn,

        ?assertEqual(ok, gen_server:call(RoomPid, {move, First, 1, 1})),
        ?assertMatch(
            {error, <<"invalid_move">>},
            gen_server:call(RoomPid, {move, First, 0, 0})
        ),

        {ok, Updated} = gen_server:call(RoomPid, state),
        ?assertEqual(1, length(Updated#game.turns))
    after
        Pid1 ! stop,
        Pid2 ! stop,
        exit(RoomPid, shutdown)
    end.
