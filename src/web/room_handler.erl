-module(room_handler).
-behaviour(cowboy_handler).

%% @doc Serves the public room HTML shell.

-export([init/2]).

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
%% @doc Serve the room HTML shell.
%% Sample usage: configured in routes as `{"/_room", room_handler, []}`.
init(Req, State) ->
    Template = utils:get_template("priv/static/gameroom/room.html"),
    Req1 = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"text/html">>},
        Template,
        Req
    ),
    {ok, Req1, State}.
