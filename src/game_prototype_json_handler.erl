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
    case auth_internal:register_account(Login, Password) of
	{ok, _} ->
	    {result, <<"ok">>};
	{error, Reason} ->
	    {error, Reason}
    end;

do_call({<<"loginAccount">>, [Login, Password], Ctx}) ->
    case http_context:login(Ctx, Login, Password) of
	{ok, Ctx1} ->
	    {result, <<"login ok">>};
	{error, Ctx1} ->
	    {error, <<"login failed">>}
    end;

do_call({<<"getCurrentAccount">>, [], Ctx}) ->
    case Ctx#http_context.account_id of
	undefined ->
	    {result, <<"account id doesn't exists">>};
	AccountId ->
	    {result, list_to_binary(integer_to_list(AccountId))}
    end;
    
do_call({Method, _Params, _Ctx}) ->
    ErrorMessage = io_lib:format("Method ~p doesn't exist", [binary_to_list(Method)]),
    {error, erlang:iolist_to_binary(ErrorMessage)}.
