-module(battleship_env_parser).
-behavior(dotenv_config_parser).

-export([get_parser/0]).

get_parser() ->
    [
        {<<"POSTGRES_DB">>, str},
        {<<"POSTGRES_USER">>, str},
        {<<"POSTGRES_PASSWORD">>, str},
        {<<"POSTGRES_PORT">>, int},
        {<<"POSTGRES_HOST">>, str}
    ].
