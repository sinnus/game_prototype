-module(game_prototype_messaging_resource).
-export([init/1,
	 charsets_provided/2,
	 allowed_methods/2,
	 process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("common.hrl").
-define(INVALID_JSON_RPC_FORMAT_MSG, <<"Invalid JSON-RPC format">>).
-define(INVALID_JSON_FORMAT_MSG, <<"Invalid JSON format">>).
-define(JSON_HANDLER_MODULE, game_prototype_json_handler).

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
    case wrq:get_req_header("content-type", RD) of
        "application/x-www-form-urlencoded" ++ _ ->
	    do_process_post(RD, Ctx);
	_ ->
	    {false, RD, Ctx}
    end.

do_process_post(RD, Ctx) ->
    {RD1, Ctx1} = http_context:ensure_session_id(RD, Ctx),
    RD2 = wrq:set_resp_header("Content-Type", "application/json; charset=utf-8", RD1),
    StreamBody = {<<"before\n">>, fun() -> feed(Ctx) end},
    {true, wrq:set_resp_body({stream, StreamBody}, RD2), Ctx1}.

feed(_Ctx) ->
    receive
	%%
	after 1000 ->
		{<<"Hello">>, done}
	end.
