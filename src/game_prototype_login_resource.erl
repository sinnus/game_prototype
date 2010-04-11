%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.
-module(game_prototype_login_resource).
-export([allowed_methods/2, process_post/2]).
-include_lib("request_html.hrl").

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

%% is_authorized(ReqData, Context) ->
%%     ReqData1 = wrq:append_to_response_body(<<"Access denied">>, ReqData),
%%     ReqData2 = wrq:set_resp_header("Content-Type", "text/html; charset=utf-8", ReqData1),
%%     {"HelloWorld", ReqData2, Context}.

%% curl -d "login=a&password=sdsss" http://localhost:8000/login.php
process_post(RD, Ctx) ->
    Data0 = wrq:req_body(RD),
    Data = mochiweb_util:parse_qs(Data0),
    Login = proplists:get_value("login", Data),
    Password = proplists:get_value("password", Data),
    do_process_post(RD, Ctx, Login, Password).

do_process_post(RD, Ctx, Login, Password) when is_list(Login) andalso is_list(Password)  ->
    {RD2, Ctx2} = case http_context:login(RD, Ctx,
					  list_to_binary(Login),
					  list_to_binary(Password)) of
		      {ok, RD1, Ctx1} ->
			  {wrq:append_to_resp_body([<<"ok">>], RD1), Ctx1};
		      {error, RD1, Ctx1} ->
			  {wrq:append_to_resp_body([<<"error">>], RD1), Ctx1}
		  end,
    {true, RD2, Ctx2};

do_process_post(RD, _Ctx, _Login, _Password) ->
    RD1 = wrq:append_to_resp_body([<<"error">>], RD),
    {true, RD1, _Ctx}.

html(ReqData, Context) ->
    Ssid = binary_to_list(Context#http_context.ssid),
    {"<html><body>Login page: " ++ Ssid ++ "</body></html>", ReqData, Context}.
