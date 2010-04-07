-module(http_session_manager_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

server_test() ->
    crypto:start(),
    uuids:start(),
    process_flag(trap_exit, true),

    {ok, Pid} = http_session_manager:start_link(600, 600),

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

    SessionPid = Context2#http_context.session_pid,
    %% Waiting when http_session will be stoped
    Result = receive 
		 {'DOWN', _, process, SessionPid, _} ->
		     ok
	     after 10000 ->
		     timeout
	     end,

    ?assert(Result =:= ok),
    
    Context3 = http_session_manager:ensure_session(Context2),
    ?assertNot(Context3#http_context.session_pid =:= Context2#http_context.session_pid),

    erlang:monitor(process, Pid),    
    http_session_manager:stop(),

    %% Waiting when http_session_manager will be stoped to start new timeout test
    receive 
	{'DOWN', _, process, Pid, _} ->
	    ok
    after 10000 ->
	    timeout
    end,

    %% Test session timeout
    {ok, _Pid2} = http_session_manager:start_link(1, 1),
    Context4 = #http_context{},
    Context5 = http_session_manager:ensure_session(Context4),
    SessionPid2 = Context5#http_context.session_pid,
    erlang:monitor(process, SessionPid2),

    Result2 = receive 
		 {'DOWN', _, process, SessionPid2, _} ->
		     ok
	     after 3000 ->
		     timeout
	     end,

    ?assert(Result2 =:= ok),
    ?assertNot(erlang:is_process_alive(Context5#http_context.session_pid)),

    %% Check account_id functions
    Context6 = #http_context{},
    Context7 = http_session_manager:ensure_session(Context6),
    Pid10 = Context7#http_context.session_pid,

    http_session:set_account_id(Pid10, 123),
    ?assert(http_session:get_account_id(Pid10) =:= 123),
    http_session:reset_account_id(Pid10),
    ?assert(http_session:get_account_id(Pid10) =:= undefined),
    
    http_session_manager:stop(),

    uuids:stop(),
    crypto:stop().
