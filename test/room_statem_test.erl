-module(room_statem_test).

-include_lib("eunit/include/eunit.hrl").

client_loop(Parent) ->
    receive
        {socket_send, Payload} ->
            Parent ! {socket_send, self(), Payload},
            client_loop(Parent);
        stop ->
            ok
    end.

room_uses_configured_rules_module_test() ->
    Parent = self(),
    Pid1 = spawn(fun() -> client_loop(Parent) end),
    Pid2 = spawn(fun() -> client_loop(Parent) end),
    Player1 = #{pid => Pid1, id => <<"p1">>, name => <<"p1">>},
    Player2 = #{pid => Pid2, id => <<"p2">>, name => <<"p2">>},
    {ok, RoomPid} = room:start_link(<<"generic-room">>, fake_rules, [Player1, Player2], #{}),
    unlink(RoomPid),
    try
        receive
            {socket_send, Pid1, #{
                type := <<"room_state">>,
                view := #{viewer := <<"p1">>, players := [<<"p1">>, <<"p2">>]}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,

        ?assertEqual(
            {error, <<"action_not_allowed">>},
            gen_statem:call(RoomPid, {action, <<"p1">>, #{type => pass}})
        ),
        ?assertEqual(
            ok, gen_statem:call(RoomPid, {action, <<"p1">>, #{type => <<"mark">>, column => 3}})
        ),
        receive
            {socket_send, _Pid, #{
                type := <<"room_update">>,
                view := #{actions := [#{player_id := <<"p1">>, payload := #{column := 3}}]}
            }} ->
                ok
        after 1000 ->
            ?assert(false)
        end,

        {ok, Snapshot} = gen_statem:call(RoomPid, state),
        ?assertMatch(
            #{actions := [#{player_id := <<"p1">>, payload := #{column := 3}}]},
            Snapshot
        )
    after
        Pid1 ! stop,
        Pid2 ! stop,
        exit(RoomPid, shutdown)
    end.
