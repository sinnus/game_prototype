%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.
-module(game_prototype_json_resource).
-export([allowed_methods/2,
	 content_types_provided/2,
	 process_post/2]).
-include_lib("request_html.hrl").

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'POST'], ReqData, Context}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_html}], RD, Ctx}.

%% hit this with
%%   curl "http://localhost:8000/json?one=two&me=pope"
html(RD, Ctx) ->
    {json_body(wrq:req_qs(RD)), RD, Ctx}.

%% hit this with
%%   curl -X POST http://localhost:8000/formjson \
%%        -d "one=two&me=pope"
process_post(RD, Ctx) ->
    Body = json_body(mochiweb_util:parse_qs(wrq:req_body(RD))),
    {true, wrq:append_to_response_body(Body, RD), Ctx}.

json_body(QS) -> mochijson:encode({struct, QS}).
