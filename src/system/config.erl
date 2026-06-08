-module(config).

-export([is_dev/0]).

-spec is_dev() -> boolean().
%% @doc Return whether the current environment is configured as development.
%% Sample usage: `SecureCookie = not config:is_dev().`
is_dev() ->
    case dotenv_config:get(<<"ENV">>) of
        <<"dev">> ->
            true;
        _ ->
            false
    end.
