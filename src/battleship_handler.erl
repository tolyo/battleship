-module(battleship_handler).
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

-record(state, {player1, player2}).
-include_lib("battleship/include/battleship.hrl").

init(Req, _State) ->
    [{<<"player">>, PlayerParam}, {<<"board">>, BoardParam}] = cowboy_req:parse_qs(Req),
    logger:info("Params: ~p, ~p~n", [PlayerParam, BoardParam]),
    NewState =
        case PlayerParam of
            <<"1">> ->
                battleship_server:add_player1(#player{
                    id = PlayerParam,
                    board = BoardParam
                });
            <<"2">> ->
                #state{player1 = undefined, player2 = 2};
            %% Log error for unexpected value
            _ ->
                logger:error("Unexpected player parameter: ~p", [PlayerParam])
        end,
    {cowboy_websocket, Req, NewState}.

websocket_init(State) ->
    self() ! json:encode(State),
    {[], State}.

websocket_handle({text, Msg}, State) ->
    {[{text, <<Msg/binary>>}], State};
websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    {[{text, Msg}], State};
websocket_info(_Info, State) ->
    {[], State}.
