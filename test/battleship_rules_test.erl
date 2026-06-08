-module(battleship_rules_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("battleship/include/rules/battleship.hrl").

player_turn_and_board_update_test() ->
    Board = battleship_board:init_board(),
    Player1 = #player{id = <<"p1">>, board = Board},
    Player2 = #player{id = <<"p2">>, board = Board},
    Match = #match{
        player_one = Player1,
        player_two = Player2,
        first_turn = <<"p1">>,
        turns = [],
        state = 'ACTIVE'
    },

    ?assertEqual(<<"p1">>, (battleship_rules:get_player_by_id(Match, <<"p1">>))#player.id),
    NextMatch = battleship_rules:next_move(Match, 1, 1),

    ?assertEqual(1, length(NextMatch#match.turns)),
    Strike = hd(NextMatch#match.turns),
    ?assertEqual(<<"p1">>, Strike#strike.id),

    OpponentBoard = NextMatch#match.player_two#player.board,
    ?assertEqual(?MISS, battleship_board:get_cell_value(OpponentBoard, 1, 1)).

hit_keeps_turn_test() ->
    EmptyBoard = battleship_board:init_board(),
    TargetBoard0 = battleship_board:update_cell_at(EmptyBoard, 1, 1, '0'),
    TargetBoard = battleship_board:update_cell_at(TargetBoard0, 1, 2, '0'),
    Player1 = #player{id = <<"p1">>, board = EmptyBoard},
    Player2 = #player{id = <<"p2">>, board = TargetBoard},
    Match = #match{
        player_one = Player1,
        player_two = Player2,
        first_turn = <<"p1">>,
        turns = [],
        state = 'ACTIVE'
    },

    HitMatch = battleship_rules:next_move(Match, 1, 1),
    FirstStrike = hd(HitMatch#match.turns),
    ?assertEqual(<<"p1">>, FirstStrike#strike.id),
    ?assertEqual('HIT', FirstStrike#strike.res),

    NextHitMatch = battleship_rules:next_move(HitMatch, 1, 2),
    SecondStrike = hd(NextHitMatch#match.turns),
    ?assertEqual(<<"p1">>, SecondStrike#strike.id),
    ?assertEqual('HIT', SecondStrike#strike.res).

sinking_unit_blocks_surrounding_cells_test() ->
    EmptyBoard = battleship_board:init_board(),
    TargetBoard = battleship_board:update_cell_at(EmptyBoard, 2, 2, '9'),
    Player1 = #player{id = <<"p1">>, board = EmptyBoard},
    Player2 = #player{id = <<"p2">>, board = TargetBoard},
    Match = #match{
        player_one = Player1,
        player_two = Player2,
        first_turn = <<"p1">>,
        turns = [],
        state = 'ACTIVE'
    },

    HitMatch = battleship_rules:next_move(Match, 2, 2),
    OpponentBoard = HitMatch#match.player_two#player.board,

    ?assertEqual(?HIT, battleship_board:get_cell_value(OpponentBoard, 2, 2)),
    ?assertEqual(?BLOCKED, battleship_board:get_cell_value(OpponentBoard, 1, 1)),
    ?assertEqual(?BLOCKED, battleship_board:get_cell_value(OpponentBoard, 1, 2)),
    ?assertEqual(?BLOCKED, battleship_board:get_cell_value(OpponentBoard, 1, 3)),
    ?assertEqual(?BLOCKED, battleship_board:get_cell_value(OpponentBoard, 2, 1)),
    ?assertEqual(?BLOCKED, battleship_board:get_cell_value(OpponentBoard, 2, 3)),
    ?assertEqual(?BLOCKED, battleship_board:get_cell_value(OpponentBoard, 3, 1)),
    ?assertEqual(?BLOCKED, battleship_board:get_cell_value(OpponentBoard, 3, 2)),
    ?assertEqual(?BLOCKED, battleship_board:get_cell_value(OpponentBoard, 3, 3)).

public_view_hides_opponent_unit_cells_test() ->
    EmptyBoard = battleship_board:init_board(),
    OwnBoard = battleship_board:update_cell_at(EmptyBoard, 1, 1, '0'),
    OpponentBoard0 = battleship_board:update_cell_at(EmptyBoard, 1, 1, '1'),
    OpponentBoard = battleship_board:update_cell_at(OpponentBoard0, 1, 2, ?HIT),
    Player1 = #player{id = <<"p1">>, board = OwnBoard},
    Player2 = #player{id = <<"p2">>, board = OpponentBoard},
    Match = #match{
        player_one = Player1,
        player_two = Player2,
        first_turn = <<"p1">>,
        turns = [],
        state = 'ACTIVE'
    },

    View = battleship_rules:public_view(#{model => Match}, <<"p1">>),
    OwnView = maps:get(own_player, View),
    OpponentView = maps:get(opponent, View),

    ?assertEqual(<<"0">>, board_cell(maps:get(board, OwnView), 1, 1)),
    ?assertEqual(<<"_">>, board_cell(maps:get(board, OpponentView), 1, 1)),
    ?assertEqual(<<"+">>, board_cell(maps:get(board, OpponentView), 1, 2)),
    ?assertNot(maps:is_key(player_one, View)),
    ?assertNot(maps:is_key(player_two, View)),
    ?assertNot(maps:is_key(first_turn, View)),
    ?assertNot(maps:is_key(current_turn, View)),
    ?assertNot(maps:is_key(turns, View)),
    ?assertNot(maps:is_key(state, View)),
    ?assertNot(maps:is_key(allowed_actions, View)).

allowed_actions_includes_move_for_current_player_test() ->
    Board = battleship_board:init_board(),
    Player1 = #player{id = <<"p1">>, board = Board},
    Player2 = #player{id = <<"p2">>, board = Board},
    Match = #match{
        player_one = Player1,
        player_two = Player2,
        first_turn = <<"p1">>,
        turns = [],
        state = 'ACTIVE'
    },

    ?assertEqual(
        [#{action => <<"move">>, target => <<"opponent_board">>}],
        battleship_rules:allowed_actions(#{model => Match}, <<"p1">>)
    ),
    ?assertEqual([], battleship_rules:allowed_actions(#{model => Match}, <<"p2">>)).

allowed_actions_removes_move_after_finish_test() ->
    Board = battleship_board:init_board(),
    Player1 = #player{id = <<"p1">>, board = Board},
    Player2 = #player{id = <<"p2">>, board = Board},
    Match = #match{
        player_one = Player1,
        player_two = Player2,
        first_turn = <<"p1">>,
        turns = [#strike{id = <<"p1">>, x = 1, y = 1, res = 'HIT'}],
        state = 'FINISHED'
    },

    ?assertEqual([], battleship_rules:allowed_actions(#{model => Match}, <<"p1">>)).

board_cell(Board, Row, Column) ->
    lists:nth(Column, lists:nth(Row, Board)).
