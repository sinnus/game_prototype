-module(performance_tests).

-include_lib("eunit/include/eunit.hrl").

-export([run/0]).

run() ->
    crypto:start(),
    uuids:start(),
    {ok, Pid} = http_session_server:start_link(),
    create_session(Pid, 100000),
    io:format(user, "End~n", []),
    Pid.

create_session(Pid, 0) ->
    ok;

create_session(Pid, Num) ->
    http_session_server:new_session(Pid),
    create_session(Pid, Num - 1).
