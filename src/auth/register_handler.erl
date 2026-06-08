-module(register_handler).
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

%% @doc Initialize Cowboy REST handling for registration requests.
%% Sample usage: configured in routes as `{"/_register", register_handler, []}`.
init(Req, State) ->
    {cowboy_rest, Req, State}.

%% @doc Allow browsers to fetch the register form and submit registration JSON.
%% Sample usage: called by Cowboy REST during method negotiation.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

%% @doc Declare the HTML representation for registration form GET requests.
%% Sample usage: called by Cowboy REST for `GET /_register`.
content_types_provided(Req, State) ->
    {[{<<"text/html">>, register_form}], Req, State}.

%%--------------------------------------------------------------------
%% GET handler using ErlyDTL template
%%--------------------------------------------------------------------
%% @doc Return the registration form HTML.
%% Sample usage: Cowboy calls `register_handler:register_form(Req, State)`.
register_form(Req, State) ->
    Template = utils:get_template("priv/static/register/register.html"),
    {Template, Req, State}.

%% @doc Declare JSON as the accepted POST body content type.
%% Sample usage: called by Cowboy REST before `register_post/2`.
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, register_post}], Req, State}.

%%--------------------------------------------------------------------
%% JSON POST handler with generic validation
%%--------------------------------------------------------------------
%% @doc Validate registration fields, create the account, and return JSON status.
%% Sample usage: POST `{"username":"ada","email":"ada@example.com","password":"secret","repeatpassword":"secret"}`.
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

            case account:create(Username, Email, Password) of
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
        {<<"username">>, [validators:required()]},
        {<<"email">>, [validators:required(), validators:email()]},
        {<<"password">>, [validators:required()]},
        {<<"repeatpassword">>, [
            validators:required(),
            validators:matches(<<"password">>, "Passwords do not match")
        ]}
    ],
    lists:foldl(
        fun({Field, Rules}, Errors) ->
            validators:validate_field(Field, Rules, Map, Errors)
        end,
        #{},
        Specs
    ).
