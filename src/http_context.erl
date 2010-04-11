%%% File    : http_context.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : 
%%% Created : 22 Nov 2009 by sinnus <sinnus@linux>
-module(http_context).

-export([ensure_session_id/2,
	 ensure_all/2,
	 login/4]).
-include("common.hrl").
-define(SESSION_COOKIE, "PHPSESSID").

% {ok, ReqData, Context}
% {error, ReqData, Context}
login(ReqData, Context, Login, Password) ->
    case auth_internal:authorize(Login, Password) of
	{ok, AccountId} ->
	    Context1 = Context#http_context{ssid = undefined, session_pid = undefined},
	    Context2 = http_session_manager:ensure_session(Context1),
	    NewSessionId = Context2#http_context.ssid,
	    ReqData1 = replace_cookie_value(ReqData, ?SESSION_COOKIE, NewSessionId),

	    http_session:set_account_id(Context2#http_context.session_pid, AccountId),
	    Context3 = Context2#http_context{account_id = AccountId},
	    {ok, ReqData1, Context3};
	{error, _Reason} ->
	    {error, ReqData, Context}
    end.

ensure_all(ReqData, Context) ->
    {ReqData1, Context1} = ensure_session_id(ReqData, Context),
    AccountId =  http_session:get_account_id(Context1#http_context.session_pid),
    Context2 = Context1#http_context{account_id = AccountId},
    {ReqData1, Context2}.

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
