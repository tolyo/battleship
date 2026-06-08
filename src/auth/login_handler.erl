-module(login_handler).
-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    login_post/2,
    login_form/2
]).

-include_lib("battleship/include/account/user.hrl").
-include_lib("battleship/include/auth/session.hrl").

%%--------------------------------------------------------------------
%% Cowboy REST callbacks
%%--------------------------------------------------------------------

%% @doc Initialize Cowboy REST handling for login requests.
%% Sample usage: configured in routes as `{"/_login", login_handler, []}`.
init(Req, State) ->
    {cowboy_rest, Req, State}.

%% @doc Allow browsers to fetch the login form and submit login JSON.
%% Sample usage: called by Cowboy REST during method negotiation.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

%% @doc Declare the HTML representation for login form GET requests.
%% Sample usage: called by Cowboy REST for `GET /_login`.
content_types_provided(Req, State) ->
    {[{<<"text/html">>, login_form}], Req, State}.

%%--------------------------------------------------------------------
%% GET handler for template
%%--------------------------------------------------------------------
%% @doc Return the login form HTML.
%% Sample usage: Cowboy calls `login_handler:login_form(Req, State)`.
login_form(Req, State) ->
    Template = utils:get_template("priv/static/login/login.html"),
    {Template, Req, State}.

%% @doc Declare JSON as the accepted POST body content type.
%% Sample usage: called by Cowboy REST before `login_post/2`.
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, login_post}], Req, State}.

%%--------------------------------------------------------------------
%% JSON POST handler with generic validation
%%--------------------------------------------------------------------
%% @doc Validate credentials, create a JWT cookie, and return JSON status.
%% Sample usage: POST `{"email":"ada@example.com","password":"secret"}` to `/_login`.
login_post(Req0, State) ->
    %% Read and decode request body
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Map = json:decode(Body),
    case is_map(Map) of
        true ->
            Errors = validate_fields(Map),

            case maps:size(Errors) of
                0 ->
                    %% Extract credentials safely
                    Email = maps:get(<<"email">>, Map),
                    Password = maps:get(<<"password">>, Map),

                    logger:info("Login attempt for Email=~p", [Email]),

                    %% Check password
                    case account:check_password(Email, Password) of
                        {ok, User} ->
                            %% JWT creation using jose
                            Now = os:system_time(second),
                            %% 1 hour expiry
                            Exp = Now + 3600,
                            Claims = #{
                                <<"sub">> => User#user.id,
                                <<"username">> => User#user.username,
                                <<"iat">> => Now,
                                <<"exp">> => Exp
                            },

                            %% Secret key from environment
                            Secret = dotenv_config:get(<<"JWT_KEY">>),
                            JWK = #{
                                <<"kty">> => <<"oct">>,
                                <<"k">> => jose_base64url:encode(Secret)
                            },
                            JWS = #{<<"alg">> => <<"HS256">>},

                            Signed = jose_jwt:sign(JWK, JWS, Claims),
                            {_, Token} = jose_jws:compact(Signed),

                            %% Set JWT as secure HTTP-only cookie
                            Opts = #{
                                path => <<"/">>,
                                http_only => false,
                                secure => not config:is_dev(),
                                max_age => 3600
                            },

                            Req2 = cowboy_req:set_resp_cookie(?AUTH_COOKIE, Token, Req1, Opts),

                            %% Return minimal JSON response
                            Resp = json:encode(#{status => <<"ok">>}),
                            Req3 = cowboy_req:reply(
                                200, #{<<"content-type">> => <<"application/json">>}, Resp, Req2
                            ),
                            {stop, Req3, State};
                        {error, invalid_credentials} ->
                            Resp = json:encode(#{
                                status => <<"error">>,
                                errors => #{
                                    <<"password">> => <<"invalid username or password">>
                                }
                            }),
                            Req2 = cowboy_req:reply(
                                400, #{<<"content-type">> => <<"application/json">>}, Resp, Req1
                            ),
                            {stop, Req2, State}
                    end;
                _ ->
                    %% Validation errors
                    Resp = json:encode(#{status => <<"error">>, errors => Errors}),
                    Req2 = cowboy_req:reply(
                        422, #{<<"content-type">> => <<"application/json">>}, Resp, Req1
                    ),
                    {stop, Req2, State}
            end;
        false ->
            Resp = json:encode(#{
                status => <<"error">>, errors => #{<<"body">> => <<"invalid_json">>}
            }),
            Req2 = cowboy_req:reply(
                400, #{<<"content-type">> => <<"application/json">>}, Resp, Req1
            ),
            {stop, Req2, State}
    end.

%%--------------------------------------------------------------------
%% Field validation
%%--------------------------------------------------------------------
validate_fields(Map) ->
    Specs = [
        {<<"email">>, [validators:required()]},
        {<<"password">>, [validators:required()]}
    ],
    lists:foldl(
        fun({Field, Rules}, Errors) ->
            validators:validate_field(Field, Rules, Map, Errors)
        end,
        #{},
        Specs
    ).
