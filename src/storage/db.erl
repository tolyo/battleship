-module(db).
-behaviour(gen_server).

-export([start_link/0, stop/0, query/1, query/2, delete_all/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(POOL_NAME, db_pool).

-type query_result() :: db_pool:query_result().
-type delete_error() ::
    invalid_table_name
    | {unexpected_result, term()}
    | {throw | error | exit, term()}
    | term().

%%% Public API %%%

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
%% @doc Start the database service and initialize its connection pool.
%% Sample usage: `{ok, Pid} = db:start_link().`
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> term().
%% @doc Stop the database service.
%% Sample usage: `db:stop().`
stop() ->
    gen_server:call(?MODULE, stop).

-spec query(epgsql:sql_query()) -> query_result().
%% @doc Run a SQL query with no bind parameters.
%% Sample usage: `db:query("SELECT now();").`
query(Sql) ->
    query(Sql, []).

-spec query(epgsql:sql_query(), [epgsql:bind_param()]) -> query_result().
%% @doc Run a SQL query with bind parameters through the configured pool.
%% Sample usage: `db:query("SELECT * FROM users WHERE email = $1", [Email]).`
query(Sql, Params) ->
    db_pool:query(?POOL_NAME, Sql, Params).

-spec delete_all(binary()) -> ok | {error, delete_error()}.
%% @doc Delete all rows from a validated table name; intended for tests and maintenance.
%% Sample usage: `db:delete_all(<<"users">>).`
delete_all(TableName) when is_binary(TableName) ->
    %% Validate table name to allow only valid SQL identifiers
    case re:run(TableName, "^[a-zA-Z_][a-zA-Z0-9_]*$") of
        {match, _} ->
            Sql = <<"DELETE FROM ", TableName/binary>>,
            try db:query(Sql, []) of
                {ok, _Any} ->
                    ok;
                {error, Error} ->
                    {error, Error};
                Other ->
                    {error, {unexpected_result, Other}}
            catch
                Class:Reason ->
                    {error, {Class, Reason}}
            end;
        nomatch ->
            {error, invalid_table_name}
    end.

%%% Callbacks %%%

-spec init([]) -> {ok, #{}}.
%% @doc Initialize environment config, dependencies, and the PostgreSQL pool.
%% Sample usage: called by OTP when `db:start_link/0` starts.
init([]) ->
    % Load .env config
    dotenv_config:init(env_parser, ["config/dev.env"]),

    Host = binary_to_list(dotenv_config:get(<<"POSTGRES_HOST">>)),
    User = binary_to_list(dotenv_config:get(<<"POSTGRES_USER">>)),
    Pass = binary_to_list(dotenv_config:get(<<"POSTGRES_PASSWORD">>)),
    Db = binary_to_list(dotenv_config:get(<<"POSTGRES_DB">>)),
    Port = dotenv_config:get(<<"POSTGRES_PORT">>),

    % initial connections
    PoolSize = 5,
    % maximum connections
    MaxSize = 10,
    ok = ensure_pool_app_started(),
    Params = #{
        host => Host,
        port => Port,
        username => User,
        password => Pass,
        database => Db
    },

    {ok, _} = db_pool:start(?POOL_NAME, PoolSize, MaxSize, Params),
    io:format("Database pool '~p' started with ~p connections.~n", [?POOL_NAME, PoolSize]),
    {ok, #{}}.

-spec handle_call(term(), {pid(), term()}, term()) ->
    {reply, ok, term()} | {stop, normal, ok, term()}.
%% @doc Handle synchronous database-service control messages.
%% Sample usage: `gen_server:call(db, stop).`
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), term()) -> {noreply, term()}.
%% @doc Ignore asynchronous messages; this service exposes query calls directly.
%% Sample usage: called by OTP for `gen_server:cast(db, Msg)`.
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(term(), term()) -> {noreply, term()}.
%% @doc Ignore unexpected process messages.
%% Sample usage: called by OTP for non-call/non-cast messages.
handle_info(_, State) ->
    {noreply, State}.

-spec terminate(term(), term()) -> ok.
%% @doc Remove the database pool when the service terminates.
%% Sample usage: called by OTP during supervisor shutdown.
terminate(_Reason, _State) ->
    _ = db_pool:stop(?POOL_NAME),
    ok.

-spec code_change(term(), term(), term()) -> {ok, term()}.
%% @doc Preserve state during hot-code upgrades.
%% Sample usage: called by OTP release handling.
code_change(_, State, _) ->
    {ok, State}.

%% Internal
-spec ensure_pool_app_started() -> ok.
ensure_pool_app_started() ->
    {ok, _} = application:ensure_all_started(epgsql),
    {ok, _} = application:ensure_all_started(pooler),
    ok.
