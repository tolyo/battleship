-module(fake_rules).
-behaviour(rules).

-export([
    prepare_player/1, init/2, handle_event/2, phase/1, public_view/2, allowed_actions/2, snapshot/1
]).

%% @doc Test rules setup hook that leaves player metadata unchanged.
%% Sample usage: `fake_rules:prepare_player(#{id => <<"p1">>}).`
prepare_player(PlayerInfo) ->
    PlayerInfo.

%% @doc Create a small generic room state for room and lobby tests.
%% Sample usage: `fake_rules:init([#{id => <<"p1">>}], #{}).`
init(Players, _Options) ->
    #{
        players => [maps:get(id, Player) || Player <- Players],
        actions => [],
        phase => active
    }.

%% @doc Apply a test action event and record it in memory.
%% Sample usage:
%% `fake_rules:handle_event(#{type => <<"mark">>, player_id => <<"p1">>,
%%     payload => #{x => 1}}, State).`
handle_event(#{type := <<"mark">>, player_id := PlayerId, payload := Payload}, State) ->
    Actions = maps:get(actions, State),
    NewState = State#{actions => [#{player_id => PlayerId, payload => Payload} | Actions]},
    {ok, NewState, [#{type => action_recorded}]};
handle_event(_Event, _State) ->
    {error, <<"unknown_event">>}.

%% @doc Return the current test room phase.
%% Sample usage: `fake_rules:phase(State).`
phase(State) ->
    maps:get(phase, State, active).

%% @doc Return a player-specific view of the test state.
%% Sample usage: `fake_rules:public_view(State, <<"p1">>).`
public_view(State, PlayerId) ->
    (snapshot(State))#{viewer => PlayerId}.

%% @doc Return the test actions available to a connected player.
%% Sample usage: `fake_rules:allowed_actions(State, <<"p1">>).`
allowed_actions(State, PlayerId) ->
    IsActivePlayer =
        phase(State) =:= active andalso lists:member(PlayerId, maps:get(players, State, [])),
    case IsActivePlayer of
        true -> [#{action => <<"mark">>, target => <<"test_state">>}];
        false -> []
    end.

%% @doc Return the serializable test state without private control fields.
%% Sample usage: `fake_rules:snapshot(State).`
snapshot(State) ->
    maps:without([phase], State).
