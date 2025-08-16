-module(battleship_db).
-behaviour(gen_server).

-export([start_link/0, stop/0, query/1, query/2]).
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

%%% Callbacks %%%

init([]) ->
    % Load .env config
    dotenv_config:init(battleship_env_parser, ["config/dev.env"]),

    Host = binary_to_list(dotenv_config:get(<<"POSTGRES_HOST">>)),
    User = binary_to_list(dotenv_config:get(<<"POSTGRES_USER">>)),
    Pass = binary_to_list(dotenv_config:get(<<"POSTGRES_PASSWORD">>)),
    Db   = binary_to_list(dotenv_config:get(<<"POSTGRES_DB">>)),
    Port = dotenv_config:get(<<"POSTGRES_PORT">>),

    PoolSize = 5, % initial connections
    MaxSize = 10, % maximum connections
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

% test_connection() ->
%     % Load env config
%     dotenv_config:init(battleship_env_parser, ["config/dev.env"]),

%     HostBin = dotenv_config:get(<<"POSTGRES_HOST">>),
%     UserBin = dotenv_config:get(<<"POSTGRES_USER">>),
%     PassBin = dotenv_config:get(<<"POSTGRES_PASSWORD">>),
%     DbBin   = dotenv_config:get(<<"POSTGRES_DB">>),
%     Port    = dotenv_config:get(<<"POSTGRES_PORT">>), % already integer from parser

%     Host = binary_to_list(HostBin),
%     User = binary_to_list(UserBin),
%     Pass = binary_to_list(PassBin),
%     Db   = binary_to_list(DbBin),

%     io:format("Connecting to ~s:~p/~s~n", [Host, Port, Db]),

%     {ok, C} = epgsql:connect(Host, User, Pass, [
%         {database, Db},
%         {port, Port}
%     ]),

%     io:format("Connected!~n"),

%     % Run a test query
%     {ok, Columns, Rows} = epgsql:squery(C, "SELECT version();"),
%     io:format("Columns: ~p~nRows: ~p~n", [Columns, Rows]),

%     epgsql:close(C),
%     ok.