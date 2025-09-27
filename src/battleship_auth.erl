-module(battleship_auth).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("battleship/include/battleship_server.hrl").

%%--------------------------------------------------------------------
%% Generic auth wrapper for protecting a handler
%%
%% The handler will only be called if the cookie is valid.
%% If valid, Claims are passed in State tuple {Args, Claims}.
%%--------------------------------------------------------------------

init(Req, {Handler, Args}) ->
    case require_auth(Req) of
        {ok, Claims, Req1} ->
            %% Call the actual handler, passing Claims in State
            Handler:init(Req1, {Args, Claims});
        {error, Req2} ->
            {stop, Req2, #{}}
    end.

%%--------------------------------------------------------------------
%% Require authentication: check cookie and verify JWT
%%--------------------------------------------------------------------
require_auth(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(?AUTH_COOKIE, 1, Cookies) of
        {_, Token} ->
            case verify_jwt(Token) of
                {ok, Claims} -> {ok, Claims, Req};
                {error, _} -> {error, unauthorized(Req)}
            end;
        false ->
            {error, unauthorized(Req)}
    end.

unauthorized(Req) ->
    Resp = json:encode(#{status => <<"error">>, reason => <<"unauthorized">>}),
    cowboy_req:reply(
        401,
        #{<<"content-type">> => <<"application/json">>},
        Resp,
        Req
    ).

%%--------------------------------------------------------------------
%% Verify a JWT using jose
%%--------------------------------------------------------------------
verify_jwt(Token) ->
    Secret = dotenv_config:get(<<"JWT_KEY">>),
    JWK = #{
        <<"kty">> => <<"oct">>,
        <<"k">> => jose_base64url:encode(Secret)
    },

    case jose_jwt:verify_strict(JWK, [<<"HS256">>], Token) of
        {true, {jose_jwt, Claims}, _Jws} ->
            Exp = maps:get(<<"exp">>, Claims, 0),
            Now = os:system_time(second),
            case Now < Exp of
                true -> {ok, Claims};
                false -> {error, expired}
            end;
        {false, Reason} ->
            {error, Reason}
    end.
