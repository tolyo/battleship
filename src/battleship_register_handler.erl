-module(battleship_register_handler).
-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json/2,
    register_form/2
]).

%%--------------------------------------------------------------------
%% Cowboy REST callbacks
%%--------------------------------------------------------------------

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

%% Provide HTML for GET
content_types_provided(Req, State) ->
    {[{<<"text/html">>, register_form}], Req, State}.

%%--------------------------------------------------------------------
%% GET handler using ErlyDTL template
%%--------------------------------------------------------------------
register_form(Req, State) ->
    Template = battleship_utils:get_template("priv/static/register/register.html"),
    {Template, Req, State}.

%% Accept JSON body only for POST
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

%%--------------------------------------------------------------------
%% JSON POST handler
%%--------------------------------------------------------------------
from_json(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case json:decode(Body) of
        {ok, Map} ->
            Username = maps:get(<<"username">>, Map),
            Email = maps:get(<<"email">>, Map),
            Password = maps:get(<<"password">>, Map),

            case battleship_user:create(Username, Email, Password) of
                {ok, UserId} ->
                    Resp = json:encode(#{status => <<"ok">>, user_id => UserId}),
                    Req2 = cowboy_req:reply(
                        201,
                        #{<<"content-type">> => <<"application/json">>},
                        Resp,
                        Req1
                    ),
                    {stop, Req2, State};
                {error, Reason} ->
                    Resp = json:encode(#{status => <<"error">>, reason => Reason}),
                    Req2 = cowboy_req:reply(
                        400,
                        #{<<"content-type">> => <<"application/json">>},
                        Resp,
                        Req1
                    ),
                    {stop, Req2, State}
            end;
        {error, Reason} ->
            Resp = json:encode(#{status => <<"error">>, reason => Reason}),
            Req2 = cowboy_req:reply(
                400,
                #{<<"content-type">> => <<"application/json">>},
                Resp,
                Req1
            ),
            {stop, Req2, State}
    end.
