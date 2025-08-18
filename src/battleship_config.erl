-module(battleship_config).

-export([is_dev/0]).

-spec is_dev() -> boolean().
is_dev() ->
    case dotenv_config:get(<<"ENV">>) of
        <<"dev">> ->
            true;
        _ ->
            false
    end.
