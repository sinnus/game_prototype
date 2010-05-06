-module(roulette_game_manager_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

server_test() ->
    crypto:start(),
    uuids:start(),

    {ok, _Pid} = roulette_game_manager:start_link(),


    uuids:stop(),
    crypto:stop().
