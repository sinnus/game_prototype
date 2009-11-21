%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(game_prototype).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    game_prototype_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    game_prototype_sup:start_link().

%% @spec start() -> ok
%% @doc Start the game_prototype server.
start() ->
    game_prototype_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(game_prototype).

%% @spec stop() -> ok
%% @doc Stop the game_prototype server.
stop() ->
    Res = application:stop(game_prototype),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
