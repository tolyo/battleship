-module(battleship_lobby_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("battleship/include/battleship.hrl").

start_services() ->
    Lobby = start_service(battleship_lobby, fun battleship_lobby:start_link/0),
    RoomSup = start_service(battleship_room_sup, fun battleship_room_sup:start_link/0),
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
    exit(Pid, shutdown);
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

            {waiting, PlayerId1} = battleship_lobby:join(Pid1, #{name => <<"p1">>}),
            {matched, PlayerId2, PlayerId1, RoomId} = battleship_lobby:join(
                Pid2,
                #{name => <<"p2">>}
            ),

            ?assert(is_binary(PlayerId1)),
            ?assert(is_binary(PlayerId2)),
            ?assert(is_binary(RoomId)),

            {ok, RoomPid} = battleship_lobby:room_pid(RoomId),
            ?assert(is_process_alive(RoomPid)),

            ?assertEqual(ok, wait_for_match_found(Pid1, RoomId)),
            {ok, Game} = battleship_room:game_state(RoomId),
            First = Game#game.first_turn,
            Opponent =
                case First of
                    PlayerId1 -> PlayerId2;
                    PlayerId2 -> PlayerId1
                end,
            ?assertEqual(
                {error, <<"not_your_turn">>}, battleship_room:move(RoomId, Opponent, 0, 0)
            ),
            ?assertEqual(ok, battleship_room:move(RoomId, First, 0, 0)),
            ?assertEqual(ok, wait_for_game_update(RoomId)),
            {ok, UpdatedGame} = battleship_room:game_state(RoomId),
            ?assertEqual(1, length(UpdatedGame#game.turns)),

            Pid1 ! stop,
            Pid2 ! stop,
            exit(RoomPid, shutdown)
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

wait_for_game_update(RoomId) ->
    receive
        {socket_send, _Pid, #{type := <<"game_update">>, room_id := RoomId}} ->
            ok;
        {socket_send, _Pid, _Other} ->
            wait_for_game_update(RoomId)
    after 1000 ->
        timeout
    end.
