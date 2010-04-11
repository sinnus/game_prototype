%%% File    : game_prototype_json_handler.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : 
%%% Created : 27 Nov 2009 by sinnus <sinnus@linux>
-module(game_prototype_json_handler).
-export([do_call/1]).
-include("common.hrl").

do_call({<<"removeFiles">>, _Params, _Ctx}) ->
    {result, {struct, [{<<"username">>, <<"фио">>}]}};

%% Params -> [Login]
do_call({<<"registerAccount">>, [Login, Password], _Ctx}) ->
    ?DEBUG("Login: ~p, password: ~p", [Login, Password]),
    case auth_internal:register_account(Login, Password) of
	{ok, _} ->
	    {result, <<"ok">>};
	{error, Reason} ->
	    {error, Reason}
    end;

do_call({<<"getSsid">>, _Params, Ctx}) ->
    {result, Ctx#http_context.ssid};

do_call({<<"fail">>, _Params, _Ctx}) ->
    _A = 0 / 0,
    {result, {struct, [{<<"username">>, <<"фио">>}]}};

do_call({Method, _Params, _Ctx}) ->
    ErrorMessage = io_lib:format("Method ~p doesn't exist", [binary_to_list(Method)]),
    {error, erlang:iolist_to_binary(ErrorMessage)}.
