-module(battleship_dashboard_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, {Args, Claims}) ->
    %% You have Claims available if needed
    Template = battleship_utils:get_template("priv/static/dashboard/dashboard.html"),
    Req1 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/html">>},
        Template,
        Req
    ),
    {ok, Req1, {Args, Claims}}.
