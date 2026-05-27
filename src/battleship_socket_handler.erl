-module(battleship_socket_handler).

%% @doc WebSocket handler for lobby matchmaking and in-room game messages.

-include_lib("battleship/include/battleship.hrl").

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-record(state, {player_name, board, player_id, room_id}).

-type ws_state() :: #state{}.
-type payload() :: map().
-type ws_frame() :: {text, iodata()}.

%% ------------------------------------------------------------------
%% Cowboy callbacks.
%% ------------------------------------------------------------------

-spec init(cowboy_req:req(), term()) -> {cowboy_websocket, cowboy_req:req(), ws_state()}.
init(Req, _State) ->
    Params = cowboy_req:parse_qs(Req),
    PlayerParam = proplists:get_value(<<"player">>, Params, <<"player">>),
    BoardParam = proplists:get_value(<<"board">>, Params, undefined),
    Board = parse_board_param(BoardParam),
    {cowboy_websocket, Req, #state{player_name = PlayerParam, board = Board}}.

-spec websocket_init(ws_state()) -> {[ws_frame()], ws_state()}.
websocket_init(State = #state{player_name = PlayerName, board = Board}) ->
    PlayerInfo = #{name => PlayerName, board => Board},
    case battleship_lobby:join(self(), PlayerInfo) of
        {waiting, PlayerId} ->
            Payload = #{type => <<"lobby_waiting">>, player_id => PlayerId},
            {[{text, json:encode(Payload)}], State#state{player_id = PlayerId}};
        {matched, PlayerId, OpponentId, RoomId} ->
            Payload = match_payload(PlayerId, OpponentId, RoomId),
            {[{text, json:encode(Payload)}], State#state{
                player_id = PlayerId,
                room_id = RoomId
            }}
    end.

-spec websocket_handle(term(), ws_state()) -> {[ws_frame()], ws_state()}.
websocket_handle({text, Msg}, State) ->
    handle_message(Msg, State);
websocket_handle(_Data, State) ->
    {[], State}.

-spec websocket_info(term(), ws_state()) -> {[ws_frame()], ws_state()}.
websocket_info({socket_send, Payload}, State) ->
    NewState = update_state_from_payload(Payload, State),
    {[{text, json:encode(Payload)}], NewState};
websocket_info(_Info, State) ->
    {[], State}.

-spec terminate(term(), cowboy_req:req(), ws_state()) -> ok.
terminate(_Reason, _Req, State) ->
    battleship_lobby:leave(self()),
    case {State#state.room_id, State#state.player_id} of
        {undefined, _} -> ok;
        {_, undefined} -> ok;
        {RoomId, PlayerId} -> battleship_room:leave(RoomId, PlayerId)
    end,
    ok.

%% ------------------------------------------------------------------
%% Private helpers.
%% ------------------------------------------------------------------

-spec handle_message(binary(), ws_state()) -> {[ws_frame()], ws_state()}.
handle_message(Msg, State = #state{room_id = RoomId, player_id = PlayerId}) ->
    try json:decode(Msg) of
        #{<<"type">> := <<"move">>, <<"row">> := Row, <<"column">> := Column} ->
            case {RoomId, PlayerId} of
                {undefined, _} ->
                    Error = #{type => <<"error">>, reason => <<"no_room">>},
                    {[{text, json:encode(Error)}], State};
                {_, undefined} ->
                    Error = #{type => <<"error">>, reason => <<"no_player">>},
                    {[{text, json:encode(Error)}], State};
                _ ->
                    _ = battleship_room:move(RoomId, PlayerId, Row, Column),
                    {[], State}
            end;
        _ ->
            Error = #{type => <<"error">>, reason => <<"unknown_message">>},
            {[{text, json:encode(Error)}], State}
    catch
        _:_ ->
            Error = #{type => <<"error">>, reason => <<"invalid_payload">>},
            {[{text, json:encode(Error)}], State}
    end.

-spec match_payload(binary(), binary(), binary()) -> payload().
match_payload(PlayerId, OpponentId, RoomId) ->
    #{
        type => <<"match_found">>,
        room_id => RoomId,
        player_id => PlayerId,
        opponent_id => OpponentId
    }.

-spec parse_board_param(binary() | undefined) -> board().
parse_board_param(undefined) ->
    battleship_game:place_fleet_random();
parse_board_param(<<>>) ->
    battleship_game:place_fleet_random();
parse_board_param(BoardParam) ->
    try
        Board = json:decode(BoardParam),
        case normalize_board(Board) of
            {ok, Normalized} -> Normalized;
            {error, _} -> battleship_game:place_fleet_random()
        end
    catch
        _:_ -> battleship_game:place_fleet_random()
    end.

-spec normalize_board(term()) -> {ok, board()} | {error, invalid_board | invalid_row}.
normalize_board(Board) when is_list(Board), length(Board) =:= 10 ->
    Rows = [normalize_row(Row) || Row <- Board],
    case
        lists:all(
            fun
                ({ok, _}) -> true;
                (_) -> false
            end,
            Rows
        )
    of
        true -> {ok, [Row || {ok, Row} <- Rows]};
        false -> {error, invalid_row}
    end;
normalize_board(_) ->
    {error, invalid_board}.

-spec normalize_row(term()) -> {ok, [grid_state()]} | {error, invalid_row | invalid_cell}.
normalize_row(Row) when is_list(Row), length(Row) =:= 10 ->
    Cells = [normalize_cell(Cell) || Cell <- Row],
    case
        lists:all(
            fun
                ({ok, _}) -> true;
                (_) -> false
            end,
            Cells
        )
    of
        true -> {ok, [Cell || {ok, Cell} <- Cells]};
        false -> {error, invalid_cell}
    end;
normalize_row(_) ->
    {error, invalid_row}.

-spec normalize_cell(term()) -> {ok, grid_state()} | {error, invalid_cell}.
normalize_cell(Cell) when is_binary(Cell) ->
    normalize_cell(binary_to_list(Cell));
normalize_cell(Cell) when is_list(Cell) ->
    case Cell of
        "_" ->
            {ok, ?EMPTY};
        "o" ->
            {ok, ?BLOCKED};
        "x" ->
            {ok, ?MISS};
        "m" ->
            {ok, ?MISS};
        "+" ->
            {ok, ?HIT};
        [Digit] when Digit >= $0, Digit =< $9 ->
            {ok, digit_atom(Digit)};
        _ ->
            {error, invalid_cell}
    end;
normalize_cell(Cell) when is_integer(Cell), Cell >= 0, Cell =< 9 ->
    {ok, digit_atom($0 + Cell)};
normalize_cell(_) ->
    {error, invalid_cell}.

-spec digit_atom($0 | $1 | $2 | $3 | $4 | $5 | $6 | $7 | $8 | $9) -> grid_state().
digit_atom($0) -> '0';
digit_atom($1) -> '1';
digit_atom($2) -> '2';
digit_atom($3) -> '3';
digit_atom($4) -> '4';
digit_atom($5) -> '5';
digit_atom($6) -> '6';
digit_atom($7) -> '7';
digit_atom($8) -> '8';
digit_atom($9) -> '9'.

-spec update_state_from_payload(payload(), ws_state()) -> ws_state().
update_state_from_payload(
    #{type := <<"match_found">>, room_id := RoomId, player_id := PlayerId}, State
) ->
    State#state{room_id = RoomId, player_id = PlayerId};
update_state_from_payload(_Payload, State) ->
    State.
