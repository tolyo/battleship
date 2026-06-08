-module(rules_registry).

%% @doc Resolves ruleset identifiers to rules modules.

-export([module_for/1]).

-spec module_for(map()) -> {ok, module()} | {error, unsupported_ruleset}.
%% @doc Resolve player setup data to the rules module that should process a room.
%% Sample usage: `rules_registry:module_for(#{ruleset => <<"battleship">>}).`
module_for(#{rules := Rules}) when is_atom(Rules) ->
    resolve_rules_module(Rules);
module_for(#{ruleset := Ruleset}) ->
    module_for_ruleset(Ruleset);
module_for(#{<<"ruleset">> := Ruleset}) ->
    module_for_ruleset(Ruleset);
module_for(#{rule_set := Ruleset}) ->
    module_for_ruleset(Ruleset);
module_for(#{<<"rule_set">> := Ruleset}) ->
    module_for_ruleset(Ruleset);
module_for(_PlayerInfo) ->
    module_for_ruleset(default_ruleset()).

-spec module_for_ruleset(term()) ->
    {ok, module()} | {error, unsupported_ruleset}.
module_for_ruleset(undefined) ->
    module_for_ruleset(default_ruleset());
module_for_ruleset(Ruleset) ->
    case ruleset_key(Ruleset) of
        {ok, Key} ->
            case maps:find(Key, configured_rulesets()) of
                {ok, Rules} -> resolve_rules_module(Rules);
                error -> {error, unsupported_ruleset}
            end;
        error ->
            {error, unsupported_ruleset}
    end.

-spec configured_rulesets() -> #{binary() => module()}.
configured_rulesets() ->
    case application:get_env(battleship, rulesets) of
        {ok, Rulesets} -> normalize_rulesets(Rulesets);
        undefined -> default_rulesets()
    end.

-spec normalize_rulesets(term()) -> #{binary() => module()}.
normalize_rulesets(Rulesets) when is_map(Rulesets) ->
    maps:fold(fun normalize_ruleset/3, #{}, Rulesets);
normalize_rulesets(Rulesets) when is_list(Rulesets) ->
    lists:foldl(
        fun
            ({Name, Rules}, Acc) -> normalize_ruleset(Name, Rules, Acc);
            (_, Acc) -> Acc
        end,
        #{},
        Rulesets
    );
normalize_rulesets(_) ->
    default_rulesets().

-spec normalize_ruleset(term(), term(), #{binary() => module()}) -> #{binary() => module()}.
normalize_ruleset(Name, Rules, Acc) when is_atom(Rules) ->
    case ruleset_key(Name) of
        {ok, Key} -> Acc#{Key => Rules};
        error -> Acc
    end;
normalize_ruleset(_Name, _Rules, Acc) ->
    Acc.

-spec ruleset_key(term()) -> {ok, binary()} | error.
ruleset_key(Name) when is_binary(Name) ->
    {ok, Name};
ruleset_key(Name) when is_atom(Name) ->
    {ok, atom_to_binary(Name, utf8)};
ruleset_key(Name) when is_list(Name) ->
    case unicode:characters_to_binary(Name) of
        Key when is_binary(Key) -> {ok, Key};
        _ -> error
    end;
ruleset_key(_) ->
    error.

-spec default_ruleset() -> binary().
default_ruleset() ->
    <<"battleship">>.

-spec default_rulesets() -> #{binary() => module()}.
default_rulesets() ->
    #{default_ruleset() => battleship_rules}.

-spec resolve_rules_module(term()) -> {ok, module()} | {error, unsupported_ruleset}.
resolve_rules_module(Rules) when is_atom(Rules) ->
    case rules_module_loaded(Rules) andalso exports_rules_callbacks(Rules) of
        true -> {ok, Rules};
        false -> {error, unsupported_ruleset}
    end;
resolve_rules_module(_) ->
    {error, unsupported_ruleset}.

-spec rules_module_loaded(module()) -> boolean().
rules_module_loaded(Rules) ->
    case code:ensure_loaded(Rules) of
        {module, Rules} -> true;
        _ -> false
    end.

-spec exports_rules_callbacks(module()) -> boolean().
exports_rules_callbacks(Rules) ->
    lists:all(
        fun({Name, Arity}) -> erlang:function_exported(Rules, Name, Arity) end,
        [
            {prepare_player, 1},
            {init, 2},
            {handle_event, 2},
            {phase, 1},
            {public_view, 2},
            {allowed_actions, 2},
            {snapshot, 1}
        ]
    ).
