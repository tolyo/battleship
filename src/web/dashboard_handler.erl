-module(dashboard_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%% @doc Serve the dashboard HTML after `session_guard` has authenticated the request.
%% Sample usage: configured in routes as `{"/_dashboard", session_guard, {dashboard_handler, []}}`.
init(Req, {Args, Claims}) ->
    %% You have Claims available if needed
    Template = utils:get_template("priv/static/dashboard/dashboard.html"),
    Req1 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/html">>},
        Template,
        Req
    ),
    {ok, Req1, {Args, Claims}}.
