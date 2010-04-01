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

ensure_auth(_ReqData, _Context) ->
    ok.

ensure_session_id(ReqData, Context) ->
    SessionId = list_to_binary(wrq:get_cookie_value(?SESSION_COOKIE, ReqData)),
    Context1 = Context#http_context{ssid = SessionId},
    Context2 = http_session_manager:ensure_session(Context1),
    NewSessionId = Context2#http_context.ssid,

    case Context2#http_context.ssid =:= SessionId of
	true ->
	    {ReqData, Context2};
	false ->
	    ReqData1 = replace_cookie_value(ReqData, ?SESSION_COOKIE, NewSessionId),
	    {ReqData1,  Context2}
    end.

%% Move to macros
replace_cookie_value(ReqData, CookieName, CookieValue) ->
    Hdr = mochiweb_cookies:cookie(CookieName, CookieValue, []),
    wrq:merge_resp_headers([Hdr], ReqData).    
