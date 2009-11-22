%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.
-module(game_prototype_resource).

-include_lib("request_html.hrl").

html(ReqData, Context) ->
    {"<html><body>Hello, new world</body></html>", ReqData, Context}.
