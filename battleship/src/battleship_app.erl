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
            {"/", battleship_handler, []}
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
