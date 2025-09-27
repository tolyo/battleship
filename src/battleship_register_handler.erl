-module(battleship_register_handler).
-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    register_post/2,
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
    {[{{<<"application">>, <<"json">>, '*'}, register_post}], Req, State}.

%%--------------------------------------------------------------------
%% JSON POST handler with generic validation
%%--------------------------------------------------------------------
register_post(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Map = json:decode(Body),
    Errors = validate_fields(Map),

    case maps:size(Errors) of
        0 ->
            Username = maps:get(<<"username">>, Map),
            Email = maps:get(<<"email">>, Map),
            Password = maps:get(<<"password">>, Map),

            logger:info("Username=~p, Email=~p", [Username, Email]),

            case battleship_user:create(Username, Email, Password) of
                {ok, UserId} ->
                    Resp = json:encode(#{status => <<"ok">>, user_id => UserId}),
                    Req2 = cowboy_req:reply(
                        201, #{<<"content-type">> => <<"application/json">>}, Resp, Req1
                    ),
                    {stop, Req2, State};
                {error, Reason} ->
                    Resp = json:encode(#{status => <<"error">>, reason => Reason}),
                    Req2 = cowboy_req:reply(
                        400, #{<<"content-type">> => <<"application/json">>}, Resp, Req1
                    ),
                    {stop, Req2, State}
            end;
        _ ->
            Resp = json:encode(#{status => <<"error">>, errors => Errors}),
            Req2 = cowboy_req:reply(
                422, #{<<"content-type">> => <<"application/json">>}, Resp, Req1
            ),
            {stop, Req2, State}
    end.

%%--------------------------------------------------------------------
%% Validation entrypoint
%%--------------------------------------------------------------------
validate_fields(Map) ->
    Specs = [
        {<<"username">>, [battleship_validators:required()]},
        {<<"email">>, [battleship_validators:required(), battleship_validators:email()]},
        {<<"password">>, [battleship_validators:required()]},
        {<<"repeatpassword">>, [
            battleship_validators:required(),
            battleship_validators:matches(<<"password">>, "Passwords do not match")
        ]}
    ],
    lists:foldl(
        fun({Field, Rules}, Errors) ->
            battleship_validators:validate_field(Field, Rules, Map, Errors)
        end,
        #{},
        Specs
    ).
