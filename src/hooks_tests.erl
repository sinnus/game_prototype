-module(hooks_tests).

-export([hook3_func/0]).
-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").

hook_test() ->
    hooks:start_link(test_hook_proc),
    Hook1Func = fun(Value) ->
			put("Hook1FuncPassed", Value)
		end,
    hooks:add(test_hook_proc, hook1, Hook1Func, 0),
    hooks:run(test_hook_proc, hook1, [passed]),
    ?assert(passed =:= get("Hook1FuncPassed")),
    hooks:delete(test_hook_proc, hook1, Hook1Func, 0),
    put("Hook1FuncPassed", undefined),
    hooks:run(test_hook_proc, hook1, [passed]),
    ?assert(passed =/= get("Hook1FuncPassed")),
    
    put("Hook1FuncPassed", undefined),
    hooks:add(test_hook_proc, hook2, Hook1Func, 0),
    Hook2Func = fun(Value) ->
			put("Hook2FuncPassed", Value)
		end,
    hooks:add(test_hook_proc, hook2, Hook2Func, 0),
    hooks:run(test_hook_proc, hook2, [passed]),
    ?assert(passed =:= get("Hook1FuncPassed")),
    ?assert(passed =:= get("Hook2FuncPassed")),

    hooks:add(test_hook_proc, hook3, ?MODULE, hook3_func, 0),
    hooks:run(test_hook_proc, hook3, []),
    ?assert(passed =:= get("Hook3FuncPassed")),

    hooks:stop(test_hook_proc).

hook3_func() ->
    put("Hook3FuncPassed", passed).
