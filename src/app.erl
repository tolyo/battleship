-module(app).

-behaviour(application).

-export([start/2, stop/1]).

%% @doc Start the OTP application by booting the root supervisor.
%% Sample usage: `application:start(battleship).`
start(_StartType, _StartArgs) ->
    root_sup:start_link().

%% @doc Stop hook for the OTP application.
%% Sample usage: called by OTP during `application:stop(battleship).`
stop(_State) ->
    ok.
