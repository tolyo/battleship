-module(validators).
-export([
    required/0,
    email/0,
    matches/2,
    validate_field/4
]).

%%--------------------------------------------------------------------
%% Validation rules
%%--------------------------------------------------------------------
%% @doc Build a rule requiring a field to be non-empty.
%% Sample usage: `validators:required().`
required() -> {required, <<"This field is required">>}.

%% @doc Build a rule requiring a field to look like an email address.
%% Sample usage: `validators:email().`
email() -> {email, <<"Invalid email format">>}.

%% @doc Build a rule requiring this field to equal another field.
%% Sample usage: `validators:matches(<<"password">>, "Passwords do not match").`
matches(OtherField, Msg) -> {matches, OtherField, list_to_binary(Msg)}.

%%--------------------------------------------------------------------
%% Run validations for one field
%%--------------------------------------------------------------------
%% @doc Apply a list of validation rules for one field and merge errors into an error map.
%% Sample usage: `validators:validate_field(<<"email">>, [validators:required()], Body, #{}).`
validate_field(Field, Rules, Map, Errors) ->
    Value = maps:get(Field, Map, <<>>),
    lists:foldl(
        fun(Rule, Acc) ->
            apply_rule(Rule, Field, Value, Map, Acc)
        end,
        Errors,
        Rules
    ).

%%--------------------------------------------------------------------
%% Rule application
%%--------------------------------------------------------------------
apply_rule({required, Msg}, Field, Value, _Map, Errors) ->
    case Value of
        <<>> -> Errors#{Field => Msg};
        _ -> Errors
    end;
apply_rule({email, Msg}, Field, Value, _Map, Errors) ->
    case Value of
        <<>> ->
            %% don't complain here, let `required` handle empties
            Errors;
        _ ->
            case re:run(Value, "^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", [{capture, none}]) of
                %% valid email
                match -> Errors;
                nomatch -> Errors#{Field => Msg};
                {error, _} -> Errors#{Field => <<"Invalid regex evaluation">>}
            end
    end;
apply_rule({matches, OtherField, Msg}, Field, Value, Map, Errors) ->
    OtherValue = maps:get(OtherField, Map, <<>>),
    case Value =:= OtherValue of
        true -> Errors;
        false -> Errors#{Field => Msg}
    end.
