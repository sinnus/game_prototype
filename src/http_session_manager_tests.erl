-module(http_session_manager_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

server_test() ->
    crypto:start(),
    uuids:start(),
    {ok, Pid} = http_session_manager:start_link(),

    Context = #http_context{},
    Context1 = http_session_manager:ensure_session(Context),
    ?assertNot(Context1#http_context.ssid =:= undefined),
    Context2 = http_session_manager:ensure_session(Context1),
    ?assert(Context1#http_context.ssid =:= Context2#http_context.ssid),
    
    http_session_manager:stop(Pid),
    uuids:stop(),
    crypto:stop().
