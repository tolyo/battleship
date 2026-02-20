-module(battleship_utils_test).

-include_lib("eunit/include/eunit.hrl").

%% Test that get_random_binary returns one of two provided values
get_random_binary_returns_one_of_two_test() ->
    Val1 = <<"value1">>,
    Val2 = <<"value2">>,
    Result = battleship_utils:get_random_binary(Val1, Val2),
    ?assert(Result =:= Val1 orelse Result =:= Val2).

%% Test that get_random_binary can return both values
get_random_binary_returns_both_values_test() ->
    Val1 = atom1,
    Val2 = atom2,
    Results = [battleship_utils:get_random_binary(Val1, Val2) || _ <- lists:seq(1, 100)],
    ?assert(lists:member(Val1, Results)),
    ?assert(lists:member(Val2, Results)).

%% Test that get_random_binary works with different types
get_random_binary_with_different_types_test() ->
    Result1 = battleship_utils:get_random_binary(1, 2),
    ?assert(Result1 =:= 1 orelse Result1 =:= 2),
    Result2 = battleship_utils:get_random_binary(<<"binary1">>, <<"binary2">>),
    ?assert(Result2 =:= <<"binary1">> orelse Result2 =:= <<"binary2">>),
    Result3 = battleship_utils:get_random_binary([1, 2, 3], [4, 5, 6]),
    ?assert(Result3 =:= [1, 2, 3] orelse Result3 =:= [4, 5, 6]).
