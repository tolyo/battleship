-module(room_events_test).

-include_lib("eunit/include/eunit.hrl").

state_payload_uses_view_field_test() ->
    View = #{phase => <<"playing">>},
    ?assertEqual(
        #{
            type => <<"room_state">>,
            event => <<"state">>,
            room_id => <<"room-1">>,
            view => View
        },
        room_events:state(<<"room-1">>, View)
    ).

update_payload_uses_view_field_test() ->
    View = #{moves => []},
    ?assertEqual(
        #{
            type => <<"room_update">>,
            event => <<"update">>,
            room_id => <<"room-1">>,
            view => View
        },
        room_events:update(<<"room-1">>, View)
    ).

match_payload_includes_generic_event_test() ->
    ?assertEqual(
        #{
            type => <<"match_found">>,
            event => <<"matched">>,
            room_id => <<"room-1">>,
            player_id => <<"p1">>,
            opponent_id => <<"p2">>,
            player_name => <<"Ada">>
        },
        room_events:match_found(<<"p1">>, <<"p2">>, <<"room-1">>, <<"Ada">>)
    ).

reconnect_info_uses_view_field_test() ->
    View = #{phase => <<"playing">>},
    ?assertEqual(
        #{
            opponent_id => <<"p2">>,
            view => View
        },
        room_events:reconnect_info(<<"p2">>, View)
    ).
