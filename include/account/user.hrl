%% @doc Account row returned from the users table.
%% Sample usage: `User#user.username`.
-record(user, {
    % UUID as text
    id :: binary(),
    username :: binary(),
    email :: binary(),
    password_hash :: binary(),
    rating :: integer(),
    created_at :: calendar:datetime()
}).

%% @doc Typed alias for an account record.
%% Sample usage: `-spec find_by_username(binary()) -> {ok, user()} | not_found.`
-type user() :: #user{}.
