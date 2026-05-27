-module(battleship_game_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("battleship/include/battleship.hrl").

player_turn_and_board_update_test() ->
    Board = battleship_board:init_board(),
    Player1 = #player{id = <<"p1">>, board = Board},
    Player2 = #player{id = <<"p2">>, board = Board},
    Game = #game{
        player_one = Player1,
        player_two = Player2,
        first_turn = <<"p1">>,
        turns = [],
        state = 'ACTIVE'
    },

    ?assertEqual(<<"p1">>, (battleship_game:get_player_by_id(Game, <<"p1">>))#player.id),
    NextGame = battleship_game:next_move(Game, 1, 1),

    ?assertEqual(1, length(NextGame#game.turns)),
    Strike = hd(NextGame#game.turns),
    ?assertEqual(<<"p1">>, Strike#strike.id),

    OpponentBoard = NextGame#game.player_two#player.board,
    ?assertEqual(?MISS, battleship_board:get_cell_value(OpponentBoard, 1, 1)).

hit_keeps_turn_test() ->
    EmptyBoard = battleship_board:init_board(),
    TargetBoard0 = battleship_board:update_cell_at(EmptyBoard, 1, 1, '0'),
    TargetBoard = battleship_board:update_cell_at(TargetBoard0, 1, 2, '0'),
    Player1 = #player{id = <<"p1">>, board = EmptyBoard},
    Player2 = #player{id = <<"p2">>, board = TargetBoard},
    Game = #game{
        player_one = Player1,
        player_two = Player2,
        first_turn = <<"p1">>,
        turns = [],
        state = 'ACTIVE'
    },

    HitGame = battleship_game:next_move(Game, 1, 1),
    FirstStrike = hd(HitGame#game.turns),
    ?assertEqual(<<"p1">>, FirstStrike#strike.id),
    ?assertEqual('HIT', FirstStrike#strike.res),

    NextHitGame = battleship_game:next_move(HitGame, 1, 2),
    SecondStrike = hd(NextHitGame#game.turns),
    ?assertEqual(<<"p1">>, SecondStrike#strike.id),
    ?assertEqual('HIT', SecondStrike#strike.res).
