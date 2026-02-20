-module(battleship_room_handler).
-behaviour(cowboy_handler).

%% @doc Serves the game room HTML shell for authenticated users.

-export([init/2]).

-spec init(cowboy_req:req(), {term(), map()}) -> {ok, cowboy_req:req(), {term(), map()}}.
init(Req, {Args, Claims}) ->
    %% You have Claims available if needed
    Template = battleship_utils:get_template("priv/static/gameroom/room.html"),
    Req1 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/html">>},
        Template,
        Req
    ),
    {ok, Req1, {Args, Claims}}.
