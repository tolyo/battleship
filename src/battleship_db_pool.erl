-module(battleship_db_pool).

-export([start/4, stop/1, query/3]).

-define(TAKE_TIMEOUT, 1000).
-define(QUEUE_MAX, 50).

-type pool_name() :: atom() | binary() | string().
-type connection_params() :: #{
    host := inet:hostname() | inet:ip_address(),
    port := inet:port_number(),
    username := string(),
    password := iodata(),
    database := string()
}.

-spec start(pool_name(), non_neg_integer(), non_neg_integer(), connection_params()) ->
    {ok, pid()} | {error, term()}.
start(PoolName0, InitCount, MaxCount, Params) ->
    PoolName = pool_name_to_atom(PoolName0),
    PoolConfig = #{
        name => PoolName,
        init_count => InitCount,
        max_count => MaxCount,
        queue_max => ?QUEUE_MAX,
        start_mfa => {epgsql, connect, [Params]},
        stop_mfa => {epgsql, close, ['$pooler_pid']}
    },
    pooler:new_pool(PoolConfig).

-spec stop(pool_name()) -> ok | {error, term()}.
stop(PoolName0) ->
    pooler:rm_pool(pool_name_to_atom(PoolName0)).

-spec query(pool_name(), epgsql:sql_query(), [epgsql:bind_param()]) ->
    epgsql:reply() | {error, term()}.
query(PoolName0, Sql, Params) ->
    PoolName = pool_name_to_atom(PoolName0),
    case pooler:take_member(PoolName, ?TAKE_TIMEOUT) of
        Connection when is_pid(Connection) ->
            query_with_connection(PoolName, Connection, Sql, Params);
        error_no_members ->
            {error, pool_overload}
    end.

query_with_connection(PoolName, Connection, Sql, Params) ->
    try epgsql:equery(Connection, Sql, Params) of
        Reply ->
            pooler:return_member(PoolName, Connection, ok),
            Reply
    catch
        Class:Reason:Stacktrace ->
            pooler:return_member(PoolName, Connection, fail),
            logger:error(
                "PostgreSQL query failed: class=~p reason=~p stacktrace=~p",
                [Class, Reason, Stacktrace]
            ),
            {error, {query_failed, Class, Reason}}
    end.

pool_name_to_atom(Name) when is_atom(Name) ->
    Name;
pool_name_to_atom(Name) when is_binary(Name) ->
    binary_to_existing_atom(Name);
pool_name_to_atom(Name) when is_list(Name) ->
    list_to_existing_atom(Name).
