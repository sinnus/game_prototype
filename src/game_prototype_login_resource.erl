%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.
-module(game_prototype_login_resource).

-include_lib("request_html.hrl").

html(ReqData, Context) ->
    {"<html><body>Login page</body></html>", ReqData, Context}.
