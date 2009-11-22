-module(performance_suite).

-export([test/0]).

test() ->
    performance_tests:run().
