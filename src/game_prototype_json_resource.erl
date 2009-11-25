%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.
-module(game_prototype_json_resource).
-export([allowed_methods/2,
	 content_types_provided/2,
	 do_call/1,
	 process_post/2]).
-include_lib("request_html.hrl").

%% JSON RPC format
%% http://en.wikipedia.org/wiki/JSON-RPC

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'POST'], ReqData, Context}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_html}], RD, Ctx}.

%% hit this with
%%   curl "http://localhost:8000/json?one=two&me=pope"
html(RD, Ctx) ->
    {json_body(wrq:req_qs(RD)), RD, Ctx}.

%% hit this with
%%   curl -X POST http://localhost:8000/json \
%%        -d "one=two&me=pope"
%% curl -X POST http://localhost:8000/json -d '{"method": "removeFiles", "id": "aa", "params": ["world", {"a": 1}]}'
process_post(RD, Ctx) ->
    JsonData = mochijson2:decode(wrq:req_body(RD)),
    case JsonData of
	{struct, Data} ->
	    Method = proplists:get_value(<<"method">>, Data),
	    Params = proplists:get_value(<<"params">>, Data),
	    Id = proplists:get_value(<<"id">>, Data),
	    if (Method =:= undefined) orelse
	       (Params =:= undefined) orelse
	       (Id =:= undefined) ->
		    ?DEBUG("Constraint violated", []);
	       true ->
		    apply(?MODULE, do_call, [{Method, Params}])
	    end;
	Data ->
	    ?DEBUG("Couldn't parse: ~p", [Data])
    end,
    {true, RD, Ctx}.

json_body(QS) ->
    mochijson:encode({struct, QS}).

do_call({<<"removeFiles">>, Params}) ->
    ?DEBUG("removeFiles: ~p", [Params]);

do_call({Method, _Params}) ->
    ?DEBUG("Method ~p doesn't exist", [Method]).
