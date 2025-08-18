-module(battleship_user_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("battleship/include/battleship.hrl").

%% Test user creation, password check, and cleanup
create_user_test_() ->
    fun() ->
        %% Setup
        {ok, _Pid} = battleship_db:start_link(),
        Username = <<"test_user">>,
        ok = battleship_db:delete_all(<<"users">>),

        %% Test: Create user
        Email = <<"test_user@email.com">>,
        Password = <<"secret123">>,
        {ok, UserId} = battleship_user:create(Username, Email, Password),
        ?assert(is_binary(UserId)),

        %% Test: Find user by username
        {ok, User} = battleship_user:find_by_username(Username),
        ?assertEqual(Username, User#user.username),

        %% Test: Valid password
        {ok, User2} = battleship_user:check_password(Username, Password),
        ?assertEqual(Username, User2#user.username),

        %% Test: Invalid password
        Result = battleship_user:check_password(Username, <<"wrongpass">>),
        ?assertMatch({error, invalid_credentials}, Result),

        %% Cleanup
        ok = battleship_db:delete_all(<<"users">>)
    end.
