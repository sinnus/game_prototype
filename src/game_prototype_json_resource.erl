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
			  RD2;
		     true ->
			  Json = case apply(?MODULE, do_call, [{Method, Params}]) of
				     {result, ResultJson} ->
					 mochijson2:encode({struct, [{<<"result">>, ResultJson},
								     {<<"error">>, null},
								     {<<"id">>, Id}]});
				     {error, ErrorJson} ->
					 mochijson2:encode({struct, [{<<"result">>, null},
								      {<<"error">>, ErrorJson},
								      {<<"id">>, Id}]})
				 end,
			  wrq:append_to_resp_body([Json], RD2)
		  end;
	      Data ->
		  ?DEBUG("Couldn't parse: ~p", [Data]),
		  RD2
    end,
    {true, RD3, Ctx1}.

do_call({<<"removeFiles">>, Params}) ->
    {result, {struct, [{<<"username">>, <<"sinnus">>}]}};

do_call({Method, _Params}) ->
    ?DEBUG("Method ~p doesn't exist", [Method]).
