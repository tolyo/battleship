%% @doc Cookie name used to store the signed authentication token.
%% Sample usage: `cowboy_req:set_resp_cookie(?AUTH_COOKIE, Token, Req, Opts)`.
-define(AUTH_COOKIE, <<"SEC_USER">>).
