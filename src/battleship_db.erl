-module(battleship_db).
-behaviour(gen_server).

-export([start_link/0, stop/0, query/1, query/2, delete_all/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(POOL_NAME, battleship_db_pool).

%%% Public API %%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

query(Sql) ->
    query(Sql, []).

query(Sql, Params) ->
    epgsql_pool:query(?POOL_NAME, Sql, Params).

%% ------------------------------------------------------------------
%% Delete all rows from a given table
%% ------------------------------------------------------------------
-spec delete_all(binary()) -> ok | {error, term()}.
delete_all(TableName) when is_binary(TableName) ->
    %% Validate table name to allow only valid SQL identifiers
    case re:run(TableName, "^[a-zA-Z_][a-zA-Z0-9_]*$") of
        {match, _} ->
            Sql = <<"DELETE FROM ", TableName/binary>>,
            case catch battleship_db:query(Sql, []) of
                {'EXIT', Reason} ->
                    {error, Reason};
                {ok, _Any} ->
                    ok;
                {error, Error} ->
                    {error, Error};
                Other ->
                    {error, {unexpected_result, Other}}
            end;
        nomatch ->
            {error, invalid_table_name}
    end.

%%% Callbacks %%%

init([]) ->
    % Load .env config
    dotenv_config:init(battleship_env_parser, ["config/dev.env"]),

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

    {ok, _} = epgsql_pool:start(?POOL_NAME, PoolSize, MaxSize, Params),
    io:format("Database pool '~p' started with ~p connections.~n", [?POOL_NAME, PoolSize]),
    {ok, #{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% Internal
ensure_pool_app_started() ->
    {ok, _} = application:ensure_all_started(epgsql_pool),
    ok.
