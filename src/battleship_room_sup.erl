-module(battleship_room_sup).
-behaviour(supervisor).

%% @doc Supervisor for dynamically created game rooms.

-export([start_link/0, start_room/3]).
-export([init/1]).

%% ------------------------------------------------------------------
%% Public API.
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_room(binary(), map(), map()) -> supervisor:startchild_ret().
start_room(RoomId, Player1, Player2) ->
    supervisor:start_child(?MODULE, [RoomId, Player1, Player2]).

%% ------------------------------------------------------------------
%% supervisor callbacks.
%% ------------------------------------------------------------------

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 10
    },
    ChildSpec = #{
        id => battleship_room,
        start => {battleship_room, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [battleship_room]
    },
    {ok, {SupFlags, [ChildSpec]}}.
