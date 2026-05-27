-module(battleship_room_handler).
-behaviour(cowboy_handler).

%% @doc Serves the public game room HTML shell.

-export([init/2]).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    Template = battleship_utils:get_template("priv/static/gameroom/room.html"),
    Req1 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/html">>},
        Template,
        Req
    ),
    {ok, Req1, State}.
