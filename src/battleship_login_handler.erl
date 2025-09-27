-module(battleship_login_handler).
-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    login_post/2,
    login_form/2
]).

-include_lib("battleship/include/battleship.hrl").
-include_lib("battleship/include/battleship_server.hrl").

%%--------------------------------------------------------------------
%% Cowboy REST callbacks
%%--------------------------------------------------------------------

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

%% Provide HTML for GET
content_types_provided(Req, State) ->
    {[{<<"text/html">>, login_form}], Req, State}.

%%--------------------------------------------------------------------
%% GET handler for template
%%--------------------------------------------------------------------
login_form(Req, State) ->
    Template = battleship_utils:get_template("priv/static/login/login.html"),
    {Template, Req, State}.

%% Accept JSON body only for POST
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, login_post}], Req, State}.

%%--------------------------------------------------------------------
%% JSON POST handler with generic validation
%%--------------------------------------------------------------------
login_post(Req0, State) ->
    %% Read and decode request body
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Map = json:decode(Body),
    Errors = validate_fields(Map),

    case maps:size(Errors) of
        0 ->
            %% Extract credentials safely
            Email    = maps:get(<<"email">>, Map),
            Password = maps:get(<<"password">>, Map),

            logger:info("Login attempt for Email=~p", [Email]),

            %% Check password
            case battleship_user:check_password(Email, Password) of
                {ok, User} ->
                    %% JWT creation using jose
                    Now = os:system_time(second),
                    Exp = Now + 3600, %% 1 hour expiry
                    Claims = #{
                        <<"sub">>      => User#user.id,
                        <<"username">> => User#user.username,
                        <<"iat">>      => Now,
                        <<"exp">>      => Exp
                    },

                    %% Secret key from environment
                    Secret = dotenv_config:get(<<"JWT_KEY">>),
                    JWK = #{
                        <<"kty">> => <<"oct">>,
                        <<"k">>   => jose_base64url:encode(Secret)
                    },
                    JWS = #{<<"alg">> => <<"HS256">>},

                    Signed = jose_jwt:sign(JWK, JWS, Claims),
                    {_, Token} = jose_jws:compact(Signed),

                    %% Set JWT as secure HTTP-only cookie
                    Opts = #{
                        path      => <<"/">>,
                        http_only => false,
                        secure    => not battleship_config:is_dev(),
                        max_age   => 3600
                    },
                    
                    Req2 = cowboy_req:set_resp_cookie(?AUTH_COOKIE, Token, Req1, Opts),

                    %% Return minimal JSON response
                    Resp = json:encode(#{status => <<"ok">>}),
                    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Resp, Req2),
                    {stop, Req3, State};

                {error, invalid_credentials} ->
                    Resp = json:encode(#{
                        status => <<"error">>,
                        errors => #{
                            <<"password">> => <<"invalid username or password">>
                        }
                    }),
                    Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, Resp, Req1),
                    {stop, Req2, State}
            end;

        _ ->
            %% Validation errors
            Resp = json:encode(#{status => <<"error">>, errors => Errors}),
            Req2 = cowboy_req:reply(422, #{<<"content-type">> => <<"application/json">>}, Resp, Req1),
            {stop, Req2, State}
    end.

%%--------------------------------------------------------------------
%% Field validation
%%--------------------------------------------------------------------
validate_fields(Map) ->
    Specs = [
        {<<"email">>,    [battleship_validators:required()]},
        {<<"password">>, [battleship_validators:required()]}
    ],
    lists:foldl(fun({Field, Rules}, Errors) ->
                        battleship_validators:validate_field(Field, Rules, Map, Errors)
                end, #{}, Specs).
