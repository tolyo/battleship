-module(rules).

%% @doc Behaviour implemented by ruleset-specific modules.

-export_type([action/0, event/0, phase/0, rule_event/0, state/0, view/0]).

%% @doc Generic lifecycle phase that room processes understand.
-type phase() :: setup | active | finished.
%% @doc Ruleset-specific state stored by the generic room process.
-type state() :: map().
%% @doc Generic command envelope passed from room processes to rulesets.
-type event() :: #{
    type := binary(),
    player_id := binary(),
    payload := map()
}.
%% @doc Ruleset-produced side effect description reserved for future room orchestration.
-type rule_event() :: map().
%% @doc Serializable player-facing ruleset view before protocol metadata is attached.
-type view() :: map().
%% @doc Authoritative action descriptor exposed to clients at the protocol boundary.
-type action() :: #{action := binary(), target => term()}.

%% @doc Normalize player setup data before room startup.
%% Sample usage: `Prepared = Rules:prepare_player(PlayerInfo).`
-callback prepare_player(map()) -> map().
%% @doc Build rules-specific state from prepared players and room options.
%% Sample usage: `RulesState = Rules:init([P1, P2], #{}).`
-callback init([map()], map()) -> state().
%% @doc Apply one generic room event to rules-specific state.
%% Sample usage: `Rules:handle_event(#{type => <<"pass">>, player_id => Id, payload => #{}}, State).`
-callback handle_event(event(), state()) -> {ok, state(), [rule_event()]} | {error, binary()}.
%% @doc Report the generic room phase represented by rules-specific state.
%% Sample usage: `Phase = Rules:phase(State).`
-callback phase(state()) -> phase().
%% @doc Return the serializable rules view visible to one player.
%% Sample usage: `View = Rules:public_view(State, PlayerId).`
-callback public_view(state(), binary()) -> view().
%% @doc Return the authoritative actions one player may currently submit.
%% Sample usage: `Actions = Rules:allowed_actions(State, PlayerId).`
-callback allowed_actions(state(), binary()) -> [action()].
%% @doc Return the complete serializable rules snapshot.
%% Sample usage: `Snapshot = Rules:snapshot(State).`
-callback snapshot(state()) -> map().
