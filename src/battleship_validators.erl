-module(battleship_validators).
-export([
    required/0,
    email/0,
    matches/2,
    validate_field/4
]).

%%--------------------------------------------------------------------
%% Validation rules
%%--------------------------------------------------------------------
required() -> {required, <<"This field is required">>}.

email() -> {email, <<"Invalid email format">>}.

matches(OtherField, Msg) -> {matches, OtherField, list_to_binary(Msg)}.

%%--------------------------------------------------------------------
%% Run validations for one field
%%--------------------------------------------------------------------
validate_field(Field, Rules, Map, Errors) ->
    Value = maps:get(Field, Map, <<>>),
    lists:foldl(fun(Rule, Acc) ->
                        apply_rule(Rule, Field, Value, Map, Acc)
                end, Errors, Rules).

%%--------------------------------------------------------------------
%% Rule application
%%--------------------------------------------------------------------
apply_rule({required, Msg}, Field, Value, _Map, Errors) ->
    case Value of
        <<>> -> Errors#{Field => Msg};
        _    -> Errors
    end;

apply_rule({email, Msg}, Field, Value, _Map, Errors) ->
    case Value of
        <<>> ->
            Errors; %% don't complain here, let `required` handle empties
        _ ->
            case re:run(Value, "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", [{capture, none}]) of
                match   -> Errors;              %% valid email
                nomatch -> Errors#{Field => Msg};
                {error, _} -> Errors#{Field => <<"Invalid regex evaluation">>}
            end
    end;
    
apply_rule({matches, OtherField, Msg}, Field, Value, Map, Errors) ->
    OtherValue = maps:get(OtherField, Map, <<>>),
    case Value =:= OtherValue of
        true  -> Errors;
        false -> Errors#{Field => Msg}
    end.