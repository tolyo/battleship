-module(battleship_dashboard_handler).
-behaviour(cowboy_rest).


-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    dashboard/2
]).

%%--------------------------------------------------------------------
%% Cowboy REST callbacks
%%--------------------------------------------------------------------

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

%% Provide HTML for GET
content_types_provided(Req, State) ->
    {[{<<"text/html">>, dashboard}], Req, State}.


%%--------------------------------------------------------------------
%% GET handler for template
%%--------------------------------------------------------------------
dashboard(Req, State) ->
    Template = battleship_utils:get_template("priv/static/dashboard/dashboard.html"),
    {Template, Req, State}.