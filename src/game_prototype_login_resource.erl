%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.
-module(game_prototype_login_resource).
-export([is_authorized/2]).
-include_lib("request_html.hrl").

is_authorized(ReqData, Context) ->
    ReqData1 = wrq:append_to_response_body(<<"Access denied">>, ReqData),
    ReqData2 = wrq:set_resp_header("Content-Type", "text/html; charset=utf-8", ReqData1),
    {"HelloWorld", ReqData2, Context}.

html(ReqData, Context) ->
    Ssid = binary_to_list(Context#http_context.ssid),
    {"<html><body>Login page: " ++ Ssid ++ "</body></html>", ReqData, Context}.
