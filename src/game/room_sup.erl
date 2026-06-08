-module(room_sup).
-behaviour(supervisor).

%% @doc Supervisor for dynamically created rooms.

-export([start_link/0, start_room/4]).
-export([init/1]).

%% ------------------------------------------------------------------
%% Public API.
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
%% @doc Start the dynamic supervisor responsible for room processes.
%% Sample usage: `{ok, SupPid} = room_sup:start_link().`
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_room(binary(), module(), [map()], map()) -> supervisor:startchild_ret().
%% @doc Start one room using a rules module, prepared players, and options.
%% Sample usage: `room_sup:start_room(<<"room-1">>, battleship_rules, [P1, P2], #{}).`
start_room(RoomId, Rules, Players, Options) ->
    supervisor:start_child(?MODULE, [RoomId, Rules, Players, Options]).

%% ------------------------------------------------------------------
%% supervisor callbacks.
%% ------------------------------------------------------------------

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
%% @doc Configure room child startup for the dynamic supervisor.
%% Sample usage: called by OTP when `room_sup:start_link/0` starts.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 10
    },
    ChildSpec = #{
        id => room,
        start => {room, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [room]
    },
    {ok, {SupFlags, [ChildSpec]}}.
