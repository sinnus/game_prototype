%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.
-module(game_prototype_login_resource).

-include_lib("request_html.hrl").

html(ReqData, Context) ->
    Ssid = binary_to_list(Context#http_context.ssid),
    {"<html><body>Login page: " ++ Ssid ++ "</body></html>", ReqData, Context}.
