-module(test_suite).

-export([test/0]).

test() ->
    eunit:test(hooks_tests),
    %eunit:test(http_session_server_tests),
    eunit:test(http_session_manager_tests),
<<<<<<< HEAD
    eunit:test(roulette_game_manager_tests).
=======
    eunit:test(roulette_game_tests).
>>>>>>> 91d7a3304d7d3df22365ca87df5cdfdc0e19cbdd
    %eunit:test(auth_server_tests).
    
