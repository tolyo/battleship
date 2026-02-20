%%%-------------------------------------------------------------------
%% @doc battleship top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(battleship_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 10
    },
    ChildSpecs = [
        % Database pool
        #{
            id => battleship_db,
            start => {battleship_db, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [battleship_db]
        },

        % HTTP server
        #{
            id => battleship_server,
            start => {battleship_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [battleship_server]
        },

        % Game room supervisor
        #{
            id => battleship_room_sup,
            start => {battleship_room_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [battleship_room_sup]
        },

        % Lobby matchmaking
        #{
            id => battleship_lobby,
            start => {battleship_lobby, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [battleship_lobby]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
