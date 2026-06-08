-module(room_events).

%% @doc Wire payload builders for room and lobby events.

-export([
    match_found/3,
    match_found/4,
    reconnect_info/2,
    rejoined/4,
    state/2,
    update/2
]).

-type room_id() :: binary().
-type player_id() :: binary().
%% @doc Player-facing room view delivered over the socket protocol.
%% Sample usage: `room_events:state(RoomId, View#{allowed_actions => Actions}).`
-type view() :: rules:view().
-type payload() :: map().

-spec state(room_id(), view()) -> payload().
%% @doc Build a state payload for the initial room view.
%% Sample usage: `room_events:state(<<"room-1">>, View).`
state(RoomId, View) ->
    view_payload(<<"room_state">>, <<"state">>, RoomId, View).

-spec update(room_id(), view()) -> payload().
%% @doc Build an update payload after a rules event changes the room view.
%% Sample usage: `room_events:update(<<"room-1">>, View).`
update(RoomId, View) ->
    view_payload(<<"room_update">>, <<"update">>, RoomId, View).

-spec rejoined(room_id(), player_id(), player_id(), view()) -> payload().
%% @doc Build a payload for a player reconnecting to an existing room.
%% Sample usage: `room_events:rejoined(RoomId, PlayerId, OpponentId, View).`
rejoined(RoomId, PlayerId, OpponentId, View) ->
    #{
        type => <<"room_joined">>,
        event => <<"joined">>,
        room_id => RoomId,
        player_id => PlayerId,
        opponent_id => OpponentId,
        view => View
    }.

-spec reconnect_info(player_id(), view()) -> payload().
%% @doc Build the return value for `room:reconnect/3`.
%% Sample usage: `room_events:reconnect_info(OpponentId, View).`
reconnect_info(OpponentId, View) ->
    #{
        opponent_id => OpponentId,
        view => View
    }.

-spec match_found(player_id(), player_id(), room_id()) -> payload().
%% @doc Build a match-found payload without display metadata.
%% Sample usage: `room_events:match_found(PlayerId, OpponentId, RoomId).`
match_found(PlayerId, OpponentId, RoomId) ->
    #{
        type => <<"match_found">>,
        event => <<"matched">>,
        room_id => RoomId,
        player_id => PlayerId,
        opponent_id => OpponentId
    }.

-spec match_found(player_id(), player_id(), room_id(), binary()) -> payload().
%% @doc Build a match-found payload with the matched player's display name.
%% Sample usage: `room_events:match_found(PlayerId, OpponentId, RoomId, <<"Ada">>).`
match_found(PlayerId, OpponentId, RoomId, PlayerName) ->
    (match_found(PlayerId, OpponentId, RoomId))#{player_name => PlayerName}.

-spec view_payload(binary(), binary(), room_id(), view()) -> payload().
view_payload(Type, Event, RoomId, View) ->
    #{
        type => Type,
        event => Event,
        room_id => RoomId,
        view => View
    }.
