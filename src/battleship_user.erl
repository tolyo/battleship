-module(battleship_user).

-export([
    create/3,
    find_by_username/1,
    check_password/2
]).

-include_lib("battleship/include/battleship.hrl").

%% @doc Create a user by calling register_user/3 in DB.
-spec create(binary(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
%% Username, Email, Password -> {ok, UserId}

create(Username, Email, Password) ->
    Sql = "SELECT register_user($1, $2, $3);",
    case battleship_db:query(Sql, [Username, Email, Password]) of
        {ok, _, [{UserId}]} ->
            {ok, UserId};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Find a user by username. Returns #user{} or not_found.
-spec find_by_username(binary()) ->
    {ok, user()} | not_found | {error, term()}.

find_by_username(Username) ->
    Sql =
        "SELECT id, username, email, password_hash, rating, created_at\n"
        "           FROM users WHERE username = $1;",
    case battleship_db:query(Sql, [Username]) of
        {ok, _, [Row]} ->
            {ok, row_to_user(Row)};
        {ok, _, []} ->
            not_found;
        {error, Error} ->
            {error, Error}
    end.

%% @doc Check a password using pgcrypto's crypt().
%% Returns {ok, #user{}} if valid, or {error, invalid_credentials}.
-spec check_password(binary(), binary()) ->
    {ok, user()} | {error, invalid_credentials} | {error, term()}.

check_password(Username, Password) ->
    Sql =
        "\n"
        "        SELECT id, username, email, password_hash, rating, created_at\n"
        "        FROM users\n"
        "        WHERE username = $1\n"
        "          AND password_hash = crypt($2, password_hash);\n"
        "    ",
    case battleship_db:query(Sql, [Username, Password]) of
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
