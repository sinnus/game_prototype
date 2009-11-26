%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.
-module(game_prototype_json_resource).
-export([init/1,
	 charsets_provided/2,
	 allowed_methods/2,
	 do_call/1,
	 process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("common.hrl").
-define(INVALID_JSON_FORMAT_MSG, <<"Invalid JSON format">>).

init(_DispatchArgs) ->
    {ok, #http_context{}}.

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

%% hit this with
%%   curl -X POST http://localhost:8000/json \
%%        -d "one=two&me=pope"
%% curl -X POST http://localhost:8000/json -d '{"method": "removeFiles", "id": "aa", "params": ["world", {"a": 1}]}'
process_post(RD, Ctx) ->
    {RD1, Ctx1} = http_context:ensure_session_id(RD, Ctx),
    JsonData = mochijson2:decode(wrq:req_body(RD)),
    RD2 = wrq:set_resp_header("Content-Type", "application/json; charset=utf-8", RD1),
    RD3 = case JsonData of
	      {struct, Data} ->
		  Method = proplists:get_value(<<"method">>, Data),
		  Params = proplists:get_value(<<"params">>, Data),
		  Id = proplists:get_value(<<"id">>, Data),
		  if (Method =:= undefined) orelse
		     (Params =:= undefined) orelse
		     (Id =:= undefined) ->
			  ?DEBUG("Constraint violated", []),
			  error_to_resp(?INVALID_JSON_FORMAT_MSG, Id, RD2);
		     true ->
			  Json = case apply(?MODULE, do_call, [{Method, Params}]) of
				     {result, ResultJson} ->
					 result_to_resp(ResultJson, Id, RD2);
				     {error, ErrorJson} ->
					 error_to_resp(ErrorJson, Id, RD2)
				 end
		  end;
	      Data ->
		  ?DEBUG("Invalid JSON format", []),
		  error_to_resp(?INVALID_JSON_FORMAT_MSG, null, RD2)
	  end,
    {true, RD3, Ctx1}.

result_to_resp(ResultJson, Id, RD) ->
    Json = mochijson2:encode({struct, [{<<"result">>, ResultJson},
				{<<"error">>, null},
				{<<"id">>, Id}]}),
    wrq:append_to_resp_body([Json], RD).
    
error_to_resp(ErrorJson, Id, RD) ->
    Json = mochijson2:encode({struct, [{<<"result">>, null},
				{<<"error">>, ErrorJson},
				{<<"id">>, Id}]}),
    wrq:append_to_resp_body([Json], RD).

do_call({<<"removeFiles">>, Params}) ->
    {result, {struct, [{<<"username">>, <<"фио">>}]}};

do_call({Method, _Params}) ->
    ?DEBUG("Method ~p doesn't exist", [Method]).
