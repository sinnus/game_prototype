%%% File    : http_context.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : 
%%% Created : 22 Nov 2009 by sinnus <sinnus@linux>
-module(http_context).

-export([ensure_session_id/2]).
-include("common.hrl").
-define(SESSION_COOKIE, "PHPSESSID").

ensure_all(ReqData, Context) ->
    {ReqData1, Context1} = ensure_session_id(ReqData, Context),
    ensure_auth(ReqData1, Context1).

ensure_auth(ReqData, Context) ->
    ok.

ensure_session_id(ReqData, Context) ->
    SessionIdValue = wrq:get_cookie_value(?SESSION_COOKIE, ReqData),
    create_or_update_ssid(ReqData, Context, SessionIdValue).

%% Internal functions
create_or_update_ssid(ReqData, Context, undefined) ->
    SessionId = http_session_server:new_session(http_session_server),
    ReqData1 = replace_cookie_value(ReqData, ?SESSION_COOKIE, SessionId),
    {ReqData1, Context#http_context{ssid = SessionId}};
    
create_or_update_ssid(ReqData, Context, SessionIdValue) ->
    SessionId = list_to_binary(SessionIdValue),
    case http_session_server:new_or_update_session(http_session_server, SessionId) of
	{update, _} ->
	    {ReqData, Context#http_context{ssid = SessionId}};
	{new, NewSessionId} ->
	    {replace_cookie_value(ReqData, ?SESSION_COOKIE, NewSessionId),
	     Context#http_context{ssid = NewSessionId}}
    end.

%% Move to macros
replace_cookie_value(ReqData, CookieName, CookieValue) ->
    Hdr = mochiweb_cookies:cookie(CookieName, CookieValue, []),
    wrq:merge_resp_headers([Hdr], ReqData).    
