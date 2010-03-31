-module(http_session_manager_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

server_test() ->
    crypto:start(),
    uuids:start(),

    {ok, _Pid} = http_session_manager:start_link(),

    Context = #http_context{},
    Context1 = http_session_manager:ensure_session(Context),
    ?assertNot(Context1#http_context.ssid =:= undefined),
    ?assertNot(Context1#http_context.session_pid =:= undefined),
    ?assert(is_pid(Context1#http_context.session_pid)),

    Context2 = http_session_manager:ensure_session(Context1),
    ?assert(Context1#http_context.ssid =:= Context2#http_context.ssid),
    ?assert(Context1#http_context.session_pid =:= Context2#http_context.session_pid),
    erlang:monitor(process, Context2#http_context.session_pid),
    http_session:stop(Context2#http_context.session_pid),

    %% Waiting when http_session will be stoped
    receive 
	{'DOWN', _Ref, process, Pid2, _Reason} ->
	    ?assert(Pid2 =:= Context2#http_context.session_pid),
	    ok
    after 10000 ->
	    timeout
    end,

    Context3 = http_session_manager:ensure_session(Context2),
    ?assertNot(Context3#http_context.session_pid =:= Context2#http_context.session_pid),
    
    http_session_manager:stop(),
    uuids:stop(),
    crypto:stop().
