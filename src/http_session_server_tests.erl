-module(http_session_server_tests).

-include_lib("eunit/include/eunit.hrl").

server_test() ->
    crypto:start(),
    uuids:start(),
    hooks:start_link(http_session_hooks),

    {ok, Pid} = http_session_server:start_link(),
    ?assertNot(http_session_server:check_session(Pid, <<"922f0a209e4bcd3206c7ebee1110b43d">>)),

    SessionId = http_session_server:new_session(Pid),
    ?assert(http_session_server:check_session(Pid, SessionId)),

    http_session_server:remove_session(Pid, SessionId),
    ?assertNot(http_session_server:check_session(Pid, SessionId)),

    {new, SessionId2} = http_session_server:new_or_update_session(Pid, <<"ffffffffffffffffffffffffffffffff">>),
    {update, SessionId3} = http_session_server:new_or_update_session(Pid, SessionId2),
    ?assert(SessionId2 =:= SessionId3),

    hooks:add(http_session_hooks, new_session, fun(SessionId) ->
						       ?assert(is_binary(SessionId)),
						       put("SessionId4New", SessionId)
					       end, 0),
    hooks:add(http_session_hooks, remove_session, fun(SessionId) ->
							  ?assert(is_binary(SessionId)),
							  put("SessionId4Remove", SessionId)
						  end, 0),

    SessionId4 = http_session_server:new_session(Pid),
    ?assert(get("SessionId4New") =:= SessionId4),
    http_session_server:remove_session(Pid, SessionId4),
    ?assert(get("SessionId4Remove") =:= SessionId4),

    http_session_server:stop(Pid),
    hooks:stop(http_session_hooks),
    uuids:stop(),
    crypto:stop().
