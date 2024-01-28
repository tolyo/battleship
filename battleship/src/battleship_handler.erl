-module(battleship_handler).
-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"text/plain">>
        },
        <<
            "\n"
            "            <h1>HELLO</h1>\n"
            "            <script> \n"
            "                window.test = 'hello';\n"
            "                console.log('handler');\n"
            "            </script>\n"
            "        "
        >>,
        Req0
    ),
    {ok, Req, Opts}.
