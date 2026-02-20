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
            {socket_send, Pid1, #{type := <<"game_state">>}} -> ok
        after 1000 ->
            ?assert(false)
        end,
        receive
            {socket_send, Pid2, #{type := <<"game_state">>}} -> ok
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
            {socket_send, _Pid, #{type := <<"game_update">>}} -> ok
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
