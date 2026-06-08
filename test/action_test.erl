-module(action_test).

-include_lib("eunit/include/eunit.hrl").

event_normalizes_submitted_action_test() ->
    ?assertEqual(
        #{type => <<"mark">>, player_id => <<"p1">>, payload => #{column => 3}},
        action:event(<<"p1">>, #{type => <<"mark">>, column => 3})
    ).

payload_prefers_nested_payload_test() ->
    ?assertEqual(
        #{column => 3},
        action:payload(#{type => <<"mark">>, payload => #{column => 3}, ignored => true})
    ).

allowed_matches_binary_atom_and_string_names_test() ->
    Allowed = [#{action => <<"mark">>}],
    ?assert(action:allowed(#{type => <<"mark">>}, Allowed)),
    ?assert(action:allowed(#{type => mark}, Allowed)),
    ?assert(action:allowed(#{type => "mark"}, Allowed)).

allowed_rejects_missing_or_unknown_names_test() ->
    Allowed = [#{action => <<"mark">>}],
    ?assertNot(action:allowed(#{type => <<"pass">>}, Allowed)),
    ?assertNot(action:allowed(#{column => 3}, Allowed)).
