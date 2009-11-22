%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(game_prototype_resource).
-export([init/1,
	 to_html/2,
	 service_available/2,
	 charsets_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

service_available(ReqData, Context) ->
    {true, ReqData, Context}.

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

to_html(ReqData, State) ->
    {"<html><body>Hello, new world</body></html>", ReqData, State}.
