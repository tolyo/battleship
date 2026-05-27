-module(battleship_server).

-behaviour(gen_server).
-include_lib("battleship/include/battleship.hrl").

%% API
-export([
    start_link/0,
    start_game/2,
    next_move/4,
    init/1,
    add_player1/1,
    add_player2/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    stop/0
]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> term().
stop() -> gen_server:call(?MODULE, stop).

-spec add_player1(#player{}) -> #game{}.
add_player1(Player) -> gen_server:call(?MODULE, {add_player1, Player}).

-spec add_player2(#player{}) -> #game{}.
add_player2(Player) -> gen_server:call(?MODULE, {add_player2, Player}).

-spec start_game(#player{}, #player{}) -> #game{}.
start_game(Player1, Player2) -> gen_server:call(?MODULE, {start_game, Player1, Player2}).

-spec next_move(#player{}, #game{}, row(), column()) -> #game{}.
next_move(Player, Game, Row, Column) ->
    gen_server:call(?MODULE, {next_move, Player, Game, Row, Column}).

%% Callbacks

-spec handle_call(term(), {pid(), term()}, #game{}) -> {reply, #game{}, #game{}}.
handle_call({add_player1, Player1}, _From, State) ->
    NewState = State#game{player_one = Player1},
    {reply, NewState, NewState};
handle_call({add_player2, Player2}, _From, State) ->
    NewState = State#game{player_two = Player2},
    {reply, NewState, NewState};
handle_call({start_game, Player1, Player2}, _From, _State) ->
    Reply = #game{
        player_one = Player1,
        player_two = Player2,
        first_turn = battleship_utils:get_random_binary(Player1#player.id, Player2#player.id),
        turns = [],
        state = 'ACTIVE'
    },
    {reply, Reply, Reply};
handle_call({next_move, _Player, Row, Column}, _From, State) ->
    % we probably want to add validation here that that the player is allowed to move
    NewState = battleship_game:next_move(State, Row, Column),
    {reply, NewState, NewState}.

-spec init([]) -> {ok, #game{}}.
init([]) -> {ok, battleship_game:init_mock_game()}.

-spec handle_cast(term(), #game{}) -> {noreply, #game{}}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(term(), #game{}) -> {noreply, #game{}}.
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(term(), #game{}) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(term(), #game{}, term()) -> {ok, #game{}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
