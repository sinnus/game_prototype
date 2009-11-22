%%% File    : http_context.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : 
%%% Created : 22 Nov 2009 by sinnus <sinnus@linux>
-module(http_context).

-export([ensure_session_id/1]).

-define(SESSION_COOKIE, "PHPSESSID").

%% Internal functions
ensure_session_id(ReqData) ->
    SessionIdValue = wrq:get_cookie_value(?SESSION_COOKIE, ReqData),
    create_or_update_ssid(ReqData, SessionIdValue).

create_or_update_ssid(ReqData, undefined) ->
    SessionId = http_session_server:new_session(http_session_server),
    replace_cookie_value(ReqData, ?SESSION_COOKIE, SessionId);
    
create_or_update_ssid(ReqData, SessionIdValue) ->
    SessionId = list_to_binary(SessionIdValue),
    case http_session_server:new_or_update_session(http_session_server, SessionId) of
	{update, _} ->
	    ReqData;
	{new, NewSessionId} ->
	    replace_cookie_value(ReqData, ?SESSION_COOKIE, NewSessionId)
    end.    

%% Move to macros
replace_cookie_value(ReqData, CookieName, CookieValue) ->
    Hdr = mochiweb_cookies:cookie(CookieName, CookieValue, []),
    wrq:merge_resp_headers([Hdr], ReqData).    
