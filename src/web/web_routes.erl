-module(web_routes).

%% @doc Cowboy route table for HTTP and WebSocket endpoints.

-export([dispatch/0, routes/0]).

-spec dispatch() -> cowboy_router:dispatch_rules().
%% @doc Compile the route table into Cowboy dispatch rules.
%% Sample usage: `#{env => #{dispatch => web_routes:dispatch()}}`.
dispatch() ->
    cowboy_router:compile(routes()).

-spec routes() -> cowboy_router:routes().
%% @doc Return the uncompiled HTTP and WebSocket route table.
%% Sample usage: `Routes = web_routes:routes().`
routes() ->
    [
        {'_', [
            {"/", cowboy_static, {file, "priv/static/index.html"}},
            {"/static/[...]", cowboy_static, {dir, "priv/static"}},

            {"/_register", register_handler, []},
            {"/_login", login_handler, []},

            {"/_dashboard", session_guard, {dashboard_handler, []}},
            {"/_room", room_handler, []},
            {"/ws", socket_handler, []},

            {"/[...]", cowboy_static, {file, "priv/static/index.html"}}
        ]}
    ].
