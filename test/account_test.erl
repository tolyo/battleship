-module(account_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("battleship/include/account/user.hrl").

%% Test user creation, password check, and cleanup
create_user_test_() ->
    fun() ->
        %% Setup
        {ok, _Pid} = db:start_link(),
        Username = <<"test_user">>,
        ok = db:delete_all(<<"users">>),

        %% Test: Create user
        Email = <<"test_user@email.com">>,
        Password = <<"secret123">>,
        {ok, UserId} = account:create(Username, Email, Password),
        ?assert(is_binary(UserId)),

        %% Test: Find user by username
        {ok, User} = account:find_by_username(Username),
        ?assertEqual(Username, User#user.username),

        %% Test: Valid password
        {ok, User2} = account:check_password(Email, Password),
        ?assertEqual(Username, User2#user.username),

        %% Test: Invalid password
        Result = account:check_password(Email, <<"wrongpass">>),
        ?assertMatch({error, invalid_credentials}, Result),

        %% Test: Invalid password
        Result = account:check_password(<<"nonuser">>, Password),
        ?assertMatch({error, invalid_credentials}, Result),

        %% Cleanup
        ok = db:delete_all(<<"users">>)
    end.
