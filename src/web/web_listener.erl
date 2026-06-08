-module(web_listener).
-behaviour(gen_server).

%% @doc Supervised Cowboy listener.

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LISTENER, http_listener).
-define(PORT, 4000).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
%% @doc Start the supervised Cowboy listener process.
%% Sample usage: `{ok, Pid} = web_listener:start_link().`
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, #{listener := atom(), port := inet:port_number()}}.
%% @doc Start Cowboy on the configured port using `web_routes`.
%% Sample usage: called by OTP when `web_listener:start_link/0` starts.
init([]) ->
    {ok, _} = cowboy:start_clear(
        ?LISTENER,
        [{port, ?PORT}],
        #{env => #{dispatch => web_routes:dispatch()}}
    ),
    {ok, #{listener => ?LISTENER, port => ?PORT}}.

-spec handle_call(term(), {pid(), term()}, map()) -> {reply, {error, unknown_call}, map()}.
%% @doc Reject synchronous calls; the listener has no public call API.
%% Sample usage: called by OTP for `gen_server:call(web_listener, Msg)`.
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
%% @doc Ignore asynchronous messages.
%% Sample usage: called by OTP for `gen_server:cast(web_listener, Msg)`.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
%% @doc Ignore unexpected process messages.
%% Sample usage: called by OTP for non-call/non-cast messages.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
%% @doc Stop the Cowboy listener on process termination.
%% Sample usage: called by OTP during supervisor shutdown.
terminate(_Reason, #{listener := Listener}) ->
    _ = cowboy:stop_listener(Listener),
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
%% @doc Preserve listener state during hot-code upgrades.
%% Sample usage: called by OTP release handling.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
