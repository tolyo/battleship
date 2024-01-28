-module(battleship_handler).
-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"text/plain">>
        },
        <<"Hello fddsfafdsaddf!">>,
        Req0
    ),
    {ok, Req, Opts}.
