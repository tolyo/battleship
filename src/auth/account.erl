-module(account).

-import(db, [query/2]).

-export([
    create/3,
    find_by_username/1,
    check_password/2
]).

-include_lib("battleship/include/account/user.hrl").

-type user_error() :: invalid_credentials | term().

-spec create(binary(), binary(), binary()) -> {ok, binary()} | {error, user_error()}.
%% @doc Create a user account through the database registration function.
%% Sample usage: `account:create(<<"ada">>, <<"ada@example.com">>, <<"secret">>).`
create(Username, Email, Password) ->
    Sql = "SELECT register_user($1, $2, $3);",
    case query(Sql, [Username, Email, Password]) of
        {ok, _, [{UserId}]} ->
            {ok, UserId};
        {error, Error} ->
            {error, Error}
    end.

-spec find_by_username(binary()) -> {ok, user()} | not_found | {error, term()}.
%% @doc Look up a user record by username.
%% Sample usage: `account:find_by_username(<<"ada">>).`
find_by_username(Username) ->
    Sql =
        "SELECT id, username, email, password_hash, rating, created_at "
        "           FROM users WHERE username = $1;",
    case query(Sql, [Username]) of
        {ok, _, [Row]} ->
            {ok, row_to_user(Row)};
        {ok, _, []} ->
            not_found;
        {error, Error} ->
            {error, Error}
    end.

-spec check_password(binary(), binary()) -> {ok, user()} | {error, user_error()}.
%% @doc Validate an email/password pair and return the matching user on success.
%% Sample usage: `account:check_password(<<"ada@example.com">>, <<"secret">>).`
check_password(Email, Password) ->
    Sql = """
        SELECT id, username, email, password_hash, rating, created_at 
        FROM users 
        WHERE email = $1 
        AND password_hash = crypt($2, password_hash); 
    """,
    case query(Sql, [Email, Password]) of
        {ok, _, [Row]} ->
            {ok, row_to_user(Row)};
        {ok, _, []} ->
            {error, invalid_credentials};
        {error, Error} ->
            {error, Error}
    end.

%% Convert DB row into #user{} record
-spec row_to_user({binary(), binary(), binary(), binary(), integer(), calendar:datetime()}) ->
    user().

row_to_user({Id, Username, Email, PasswordHash, Rating, CreatedAt}) ->
    #user{
        id = Id,
        username = Username,
        email = Email,
        password_hash = PasswordHash,
        rating = Rating,
        created_at = CreatedAt
    }.
