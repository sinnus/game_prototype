%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the game_prototype application.

-module(game_prototype_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for game_prototype.
start(_Type, _StartArgs) ->
    game_prototype_deps:ensure(),
    game_prototype_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for game_prototype.
stop(_State) ->
    ok.
