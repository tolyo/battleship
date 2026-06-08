-module(root_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%% @doc Start the root supervisor registered as `root_sup`.
%% Sample usage: `{ok, Pid} = root_sup:start_link().`
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Build the supervision tree for storage, room services, and HTTP.
%% Sample usage: called by `supervisor:start_link/3`; not normally invoked directly.
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 10
    },
    ChildSpecs = [
        % Database pool.
        #{
            id => db,
            start => {db, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [db]
        },

        % Room supervisor.
        #{
            id => room_sup,
            start => {room_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [room_sup]
        },

        % Lobby matchmaking.
        #{
            id => lobby,
            start => {lobby, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [lobby]
        },

        % HTTP listener.
        #{
            id => web_listener,
            start => {web_listener, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [web_listener]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
