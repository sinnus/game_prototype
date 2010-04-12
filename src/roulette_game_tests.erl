-module(roulette_game_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

server_test() ->
    crypto:start(),
    uuids:start(),

    uuids:stop(),
    crypto:stop().
