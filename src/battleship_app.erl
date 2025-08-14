%%%-------------------------------------------------------------------
%% @doc battleship public API
%% @end
%%%-------------------------------------------------------------------

-module(battleship_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Port = 4000,
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {file, "priv/static/index.html"}},
            %% mock game matcher for now
            {"/ws", battleship_handler, []},
            {"/static/[...]", cowboy_static, {dir, "priv/static"}},

            %% public 
            {"/register", battleship_register_handler, []},
            {"/login", battleship_login_handler, []},
            {"/[...]", cowboy_static, {file, "priv/static/index.html"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    battleship_sup:start_link().

stop(_State) ->
    ok.
