-module(auth_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("auth_server.hrl").

server_test() ->
    {ok, Pid} = auth_server:start_link(),

    auth_server:register_new_session(Pid, "session1", "username1"),
    ?assert(auth_server:check_by_ssid(Pid, "session1")),
    ?assertNot(auth_server:check_by_ssid(Pid, "session2")),

    auth_server:register_new_session(Pid, "session1", "username1"),
    ?assert(auth_server:check_by_ssid(Pid, "session1")),

    check_sessions(auth_server:get_all_sessions(Pid, "username1"),
		   ["session1"]),

    auth_server:register_new_session(Pid, "session2", "username1"),
    ?assert(auth_server:check_by_ssid(Pid, "session1")),
    ?assert(auth_server:check_by_ssid(Pid, "session2")),
    check_sessions(auth_server:get_all_sessions(Pid, "username1"),
		   ["session1", "session2"]),

    auth_server:register_new_session(Pid, "session3", "username2"),
    check_sessions(auth_server:get_all_sessions(Pid, "username2"),
		   ["session3"]),

    auth_server:register_new_session(Pid, "session4", "username2"),
    check_sessions(auth_server:get_all_sessions(Pid, "username2"),
		   ["session3", "session4"]),

    auth_server:register_new_session(Pid, "session1", "username2"),
    check_sessions(auth_server:get_all_sessions(Pid, "username2"),
		   ["session1", "session3", "session4"]),

    auth_server:remove_session(Pid, "session1"),
    check_sessions(auth_server:get_all_sessions(Pid, "username2"),
		   ["session3", "session4"]),    

    auth_server:remove_session(Pid, "session2"),
    ?assertNot(auth_server:check_by_ssid(Pid, "session2")),
    check_sessions(auth_server:get_all_sessions(Pid, "username1"),
		   []),    

    auth_server:remove_session(Pid, "session3"),
    auth_server:remove_session(Pid, "session4"),
    check_sessions(auth_server:get_all_sessions(Pid, "username2"),
		   []),

    auth_server:register_new_session(Pid, "session10", "sinnus"),
    Login = auth_server:get_login_by_ssid(Pid, "session10"),
    ?assert(Login =:= "sinnus"),
    LoginUndef = auth_server:get_login_by_ssid(Pid, "unknown"),
    ?assert(LoginUndef =:= undefined),

    auth_server:stop(Pid),
    ok.

check_sessions(AuthInfos, ExpectedSessionIds) ->
    if
	length(AuthInfos) =/= length(ExpectedSessionIds) ->
	    ?debugFmt("AuthInfos: ~p length doesn't equal length of ExpectedSessionIds: ~p",
		      [AuthInfos, ExpectedSessionIds]),
	    ?assert(false);
	true ->
	    ok
    end,

    lists:map(fun(AuthInfo) ->
		      case lists:any(fun(SessionId) ->
						 AuthInfo#auth_info.ssid =:= SessionId
				     end,
				     ExpectedSessionIds) of
			  true ->
			      true;
			  false ->
			      log_session_ids(AuthInfo#auth_info.login, AuthInfos, ExpectedSessionIds),
			      ?assert(false)
		      end
	      end,
	      AuthInfos).

log_session_ids(Login, AuthInfos, ExpectedSessionIds) ->
    ?debugFmt("Login: ~p, AuthInfos: ~p, Expected SessionIds: ~p", [Login,
								    AuthInfos,
								    ExpectedSessionIds]).
