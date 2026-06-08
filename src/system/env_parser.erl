-module(env_parser).
-behavior(dotenv_config_parser).

-export([get_parser/0]).

%% @doc Describe supported dotenv keys and their expected value types.
%% Sample usage: `dotenv_config:init(env_parser, ["config/dev.env"]).`
get_parser() ->
    [
        {<<"ENV">>, str},
        {<<"JWT_KEY">>, str},
        {<<"POSTGRES_DB">>, str},
        {<<"POSTGRES_USER">>, str},
        {<<"POSTGRES_PASSWORD">>, str},
        {<<"POSTGRES_PORT">>, int},
        {<<"POSTGRES_HOST">>, str}
    ].
