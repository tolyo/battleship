-module(battleship_board_test).

-include_lib("eunit/include/eunit.hrl").

init_board_test() ->
    [
        % should create rows
        ?_assertEqual(10, length(battleship_board:init_board())),
        % should create columns
        ?_assertEqual(10, length(hd(battleship_board:init_board())))
    ].
