%%% File    : http_context.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : 
%%% Created : 22 Nov 2009 by sinnus <sinnus@linux>
-module(http_context).

-export([ensure_session_id/2, ensure_all/2]).
-include("common.hrl").
-define(SESSION_COOKIE, "PHPSESSID").

ensure_all(ReqData, Context) ->
    {ReqData1, Context1} = ensure_session_id(ReqData, Context),
    http_session:get_account_id(Context1#http_context.session_pid),
    {ReqData1, Context1}.

ensure_session_id(ReqData, Context) ->
    SessionId = get_session_id(wrq:get_cookie_value(?SESSION_COOKIE, ReqData)),

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

get_session_id(CookieValue) when is_list(CookieValue) ->
    list_to_binary(CookieValue);

get_session_id(_CookieValue) ->
    undefined.

%% Move to macros
replace_cookie_value(ReqData, CookieName, CookieValue) ->
    Hdr = mochiweb_cookies:cookie(CookieName, CookieValue, []),
    wrq:merge_resp_headers([Hdr], ReqData).    
