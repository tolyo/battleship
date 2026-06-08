-module(lobby_test).

-include_lib("eunit/include/eunit.hrl").

start_services() ->
    Lobby = start_service(lobby, fun lobby:start_link/0),
    RoomSup = start_service(room_sup, fun room_sup:start_link/0),
    {Lobby, RoomSup}.

stop_services({Lobby, RoomSup}) ->
    stop_service(RoomSup),
    stop_service(Lobby).

start_service(Name, StartFun) ->
    case whereis(Name) of
        undefined ->
            {ok, Pid} = StartFun(),
            unlink(Pid),
            {Pid, true};
        Pid ->
            {Pid, false}
    end.

stop_service({Pid, true}) ->
    Ref = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, _Reason} -> ok
    after 1000 ->
        ok
    end;
stop_service({_Pid, false}) ->
    ok.

client_loop(Parent) ->
    receive
        {socket_send, Payload} ->
            Parent ! {socket_send, self(), Payload},
            client_loop(Parent);
        stop ->
            ok
    end.

lobby_match_test_() ->
    {setup, fun start_services/0, fun stop_services/1, fun(_) ->
        fun() ->
            Parent = self(),
            Pid1 = spawn(fun() -> client_loop(Parent) end),
            Pid2 = spawn(fun() -> client_loop(Parent) end),

            {waiting, PlayerId1} = lobby:join(Pid1, #{name => <<"p1">>}),
            {matched, PlayerId2, PlayerId1, RoomId} = lobby:join(
                Pid2,
                #{name => <<"p2">>}
            ),

            ?assert(is_binary(PlayerId1)),
            ?assert(is_binary(PlayerId2)),
            ?assert(is_binary(RoomId)),

            {ok, RoomPid} = lobby:room_pid(RoomId),
            ?assert(is_process_alive(RoomPid)),

            ?assertEqual(ok, wait_for_match_found(Pid1, RoomId)),
            Pid3 = spawn(fun() -> client_loop(Parent) end),
            ?assertMatch(
                {ok, #{
                    view := #{
                        phase := <<"playing">>,
                        allowed_actions := _
                    },
                    opponent_id := PlayerId2
                }},
                room:reconnect(RoomId, PlayerId1, Pid3)
            ),
            room:leave(RoomId, PlayerId1, Pid1),

            {ok, Snapshot} = room:state(RoomId),
            First = maps:get(first_turn, Snapshot),
            Opponent =
                case First of
                    PlayerId1 -> PlayerId2;
                    PlayerId2 -> PlayerId1
                end,
            ?assertEqual(
                {error, <<"action_not_allowed">>}, room:submit(RoomId, Opponent, move_action(0, 0))
            ),
            ?assertEqual(ok, room:submit(RoomId, First, move_action(0, 0))),
            ?assertEqual(ok, wait_for_room_update_from(Pid3, RoomId)),
            {ok, UpdatedSnapshot} = room:state(RoomId),
            ?assertEqual(1, length(maps:get(turns, UpdatedSnapshot))),
            ?assertEqual({error, room_not_found}, room:state(<<"missing-room">>)),

            Pid1 ! stop,
            Pid2 ! stop,
            Pid3 ! stop,
            exit(RoomPid, shutdown)
        end
    end}.

lobby_keeps_different_rule_sets_in_separate_queues_test_() ->
    {setup, fun start_services/0, fun stop_services/1, fun(_) ->
        fun() ->
            Parent = self(),
            BattleshipPid1 = spawn(fun() -> client_loop(Parent) end),
            FakePid1 = spawn(fun() -> client_loop(Parent) end),
            FakePid2 = spawn(fun() -> client_loop(Parent) end),
            BattleshipPid2 = spawn(fun() -> client_loop(Parent) end),

            {waiting, BattleshipPlayer1} = lobby:join(BattleshipPid1, #{
                name => <<"b1">>, ruleset => <<"battleship">>
            }),
            {waiting, FakePlayer1} = lobby:join(FakePid1, #{
                name => <<"f1">>, rules => fake_rules
            }),
            {matched, FakePlayer2, FakePlayer1, FakeRoomId} = lobby:join(FakePid2, #{
                name => <<"f2">>, rules => fake_rules
            }),
            {matched, BattleshipPlayer2, BattleshipPlayer1, BattleshipRoomId} = lobby:join(
                BattleshipPid2,
                #{name => <<"b2">>, ruleset => <<"battleship">>}
            ),

            ?assert(is_binary(FakePlayer2)),
            ?assert(is_binary(BattleshipPlayer2)),
            ?assert(FakeRoomId =/= BattleshipRoomId),

            {ok, FakeRoomPid} = lobby:room_pid(FakeRoomId),
            {ok, BattleshipRoomPid} = lobby:room_pid(BattleshipRoomId),

            {ok, FakeSnapshot} = room:state(FakeRoomId),
            ?assertMatch(#{players := [FakePlayer1, FakePlayer2]}, FakeSnapshot),

            {ok, BattleshipSnapshot} = room:state(BattleshipRoomId),
            ?assertMatch(#{phase := <<"playing">>, turns := []}, BattleshipSnapshot),

            BattleshipPid1 ! stop,
            BattleshipPid2 ! stop,
            FakePid1 ! stop,
            FakePid2 ! stop,
            exit(FakeRoomPid, shutdown),
            exit(BattleshipRoomPid, shutdown)
        end
    end}.

lobby_matches_configured_ruleset_test_() ->
    {setup, fun start_services/0, fun stop_services/1, fun(_) ->
        fun() ->
            with_rulesets(
                [{<<"fake">>, fake_rules}, {<<"battleship">>, battleship_rules}],
                fun() ->
                    Parent = self(),
                    Pid1 = spawn(fun() -> client_loop(Parent) end),
                    Pid2 = spawn(fun() -> client_loop(Parent) end),

                    {waiting, Player1} = lobby:join(Pid1, #{
                        name => <<"f1">>, ruleset => <<"fake">>
                    }),
                    {matched, Player2, Player1, RoomId} = lobby:join(Pid2, #{
                        name => <<"f2">>, ruleset => <<"fake">>
                    }),

                    ?assert(is_binary(Player2)),
                    {ok, RoomPid} = lobby:room_pid(RoomId),
                    {ok, Snapshot} = room:state(RoomId),
                    ?assertMatch(#{players := [Player1, Player2]}, Snapshot),

                    Pid1 ! stop,
                    Pid2 ! stop,
                    exit(RoomPid, shutdown)
                end
            )
        end
    end}.

lobby_rejects_unknown_ruleset_test_() ->
    {setup, fun start_services/0, fun stop_services/1, fun(_) ->
        fun() ->
            Parent = self(),
            Pid = spawn(fun() -> client_loop(Parent) end),

            ?assertEqual(
                {error, unsupported_ruleset},
                lobby:join(Pid, #{name => <<"p1">>, ruleset => <<"checkers">>})
            ),

            ?assertEqual([], lobby:list_rooms()),
            Pid ! stop
        end
    end}.

wait_for_match_found(Pid, RoomId) ->
    receive
        {socket_send, Pid, #{type := <<"match_found">>, room_id := RoomId}} ->
            ok;
        {socket_send, Pid, _Other} ->
            wait_for_match_found(Pid, RoomId)
    after 1000 ->
        timeout
    end.

with_rulesets(Rulesets, Fun) ->
    Previous = application:get_env(battleship, rulesets),
    application:set_env(battleship, rulesets, Rulesets),
    try
        Fun()
    after
        restore_rulesets(Previous)
    end.

restore_rulesets({ok, Rulesets}) ->
    application:set_env(battleship, rulesets, Rulesets);
restore_rulesets(undefined) ->
    application:unset_env(battleship, rulesets).

wait_for_room_update_from(Pid, RoomId) ->
    receive
        {socket_send, Pid, #{type := <<"room_update">>, room_id := RoomId}} ->
            ok;
        {socket_send, Pid, _Other} ->
            wait_for_room_update_from(Pid, RoomId)
    after 1000 ->
        timeout
    end.

move_action(Row, Column) ->
    #{type => <<"move">>, row => Row, column => Column}.
