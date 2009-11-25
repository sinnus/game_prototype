%% Callback module for webmachine
-export([init/1,
	 to_html/2,
	 service_available/2,
	 charsets_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("common.hrl").

init(_DispatchArgs) ->
    {ok, #http_context{}}.

service_available(ReqData, Context) ->
    {true, ReqData, Context}.

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

to_html(ReqData, Context) ->
    ?DEBUG("SAAAAA", []),
    {ReqData1, Context1} = http_context:ensure_session_id(ReqData, Context),
    html(ReqData1, Context1).
