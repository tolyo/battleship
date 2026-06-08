-module(action).

%% @doc Helpers for normalizing client actions before they reach rules modules.

-export([allowed/2, event/2, payload/1, type/1]).

-type player_id() :: binary().
-type submitted() :: map().
-type allowed_action() :: map().

-spec event(player_id(), submitted()) -> rules:event().
%% @doc Convert a submitted client action into the generic rules event envelope.
%% Sample usage: `action:event(<<"p1">>, #{type => <<"mark">>, column => 3}).`
event(PlayerId, Action) ->
    #{type => type(Action), player_id => PlayerId, payload => payload(Action)}.

-spec allowed(submitted(), [allowed_action()]) -> boolean().
%% @doc Return whether a submitted action name appears in the allowed-action list.
%% Sample usage: `action:allowed(#{type => <<"move">>}, [#{action => <<"move">>}]).`
allowed(Action, AllowedActions) ->
    ActionType = type(Action),
    lists:any(
        fun(AllowedAction) ->
            allowed_type(AllowedAction) =:= ActionType
        end,
        AllowedActions
    ).

-spec type(submitted()) -> binary().
%% @doc Read and normalize the submitted action name as a binary.
%% Sample usage: `<<"move">> = action:type(#{<<"type">> => <<"move">>}).`
type(#{type := Type}) ->
    normalize_type(Type);
type(#{<<"type">> := Type}) ->
    normalize_type(Type);
type(#{action := Type}) ->
    normalize_type(Type);
type(#{<<"action">> := Type}) ->
    normalize_type(Type);
type(_) ->
    <<"unknown">>.

-spec payload(submitted()) -> map().
%% @doc Return the submitted action payload without the action-name fields.
%% Sample usage: `#{row := 0} = action:payload(#{type => <<"move">>, row => 0}).`
payload(#{payload := Payload}) when is_map(Payload) ->
    Payload;
payload(#{<<"payload">> := Payload}) when is_map(Payload) ->
    Payload;
payload(#{move := Move}) when is_map(Move) ->
    Move;
payload(#{<<"move">> := Move}) when is_map(Move) ->
    Move;
payload(Action) ->
    maps:without([type, <<"type">>, action, <<"action">>, payload, <<"payload">>], Action).

-spec allowed_type(allowed_action()) -> binary() | undefined.
allowed_type(#{action := Type}) ->
    normalize_type(Type);
allowed_type(#{<<"action">> := Type}) ->
    normalize_type(Type);
allowed_type(_) ->
    undefined.

-spec normalize_type(term()) -> binary().
normalize_type(Type) when is_atom(Type) ->
    atom_to_binary(Type, utf8);
normalize_type(Type) when is_binary(Type) ->
    Type;
normalize_type(Type) when is_list(Type) ->
    case unicode:characters_to_binary(Type) of
        Action when is_binary(Action) -> Action;
        _ -> <<"unknown">>
    end;
normalize_type(_) ->
    <<"unknown">>.
