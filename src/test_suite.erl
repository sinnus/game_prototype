-module(test_suite).

-export([test/0]).

test() ->
    eunit:test(hooks_tests),
    %eunit:test(http_session_server_tests),
    eunit:test(http_session_manager_tests).
    %eunit:test(auth_server_tests).
    
