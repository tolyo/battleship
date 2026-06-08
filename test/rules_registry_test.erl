-module(rules_registry_test).

-include_lib("eunit/include/eunit.hrl").

default_ruleset_is_battleship_test() ->
    ?assertEqual({ok, battleship_rules}, rules_registry:module_for(#{})).

battleship_ruleset_resolves_to_battleship_rules_test() ->
    ?assertEqual({ok, battleship_rules}, rules_registry:module_for(#{ruleset => <<"battleship">>})).

binary_ruleset_key_resolves_to_battleship_rules_test() ->
    ?assertEqual(
        {ok, battleship_rules}, rules_registry:module_for(#{<<"ruleset">> => <<"battleship">>})
    ).

battleship_rule_set_resolves_to_battleship_rules_test() ->
    ?assertEqual(
        {ok, battleship_rules}, rules_registry:module_for(#{rule_set => <<"battleship">>})
    ).

binary_rule_set_key_resolves_to_battleship_rules_test() ->
    ?assertEqual(
        {ok, battleship_rules}, rules_registry:module_for(#{<<"rule_set">> => <<"battleship">>})
    ).

explicit_rules_module_is_allowed_test() ->
    ?assertEqual({ok, fake_rules}, rules_registry:module_for(#{rules => fake_rules})).

explicit_non_rules_module_is_rejected_test() ->
    ?assertEqual({error, unsupported_ruleset}, rules_registry:module_for(#{rules => lists})).

configured_ruleset_resolves_to_configured_module_test() ->
    with_rulesets(
        [{<<"test">>, fake_rules}],
        fun() ->
            ?assertEqual({ok, fake_rules}, rules_registry:module_for(#{ruleset => <<"test">>})),
            ?assertEqual({ok, fake_rules}, rules_registry:module_for(#{ruleset => test})),
            ?assertEqual({ok, fake_rules}, rules_registry:module_for(#{ruleset => "test"}))
        end
    ).

configured_non_rules_module_is_rejected_test() ->
    with_rulesets(
        [{<<"broken">>, lists}],
        fun() ->
            ?assertEqual(
                {error, unsupported_ruleset}, rules_registry:module_for(#{ruleset => <<"broken">>})
            )
        end
    ).

unknown_ruleset_is_rejected_test() ->
    ?assertEqual(
        {error, unsupported_ruleset}, rules_registry:module_for(#{ruleset => <<"checkers">>})
    ).

with_rulesets(Rulesets, Fun) ->
    Previous = application:get_env(battleship, rulesets),
    application:set_env(battleship, rulesets, Rulesets),
    try
        Fun()
    after
        restore_rulesets(Previous)
    end.

restore_rulesets({ok, Rulesets}) ->
    application:set_env(battleship, rulesets, Rulesets);
restore_rulesets(undefined) ->
    application:unset_env(battleship, rulesets).
