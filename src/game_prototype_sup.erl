%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the game_prototype application.

-module(game_prototype_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
	    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    {ok, Dispatch} = file:consult(filename:join(
				    [filename:dirname(code:which(?MODULE)),
				     "..", "priv", "dispatch.conf"])),
    WebConfig = [
		 {ip, Ip},
		 {port, 8000},
                 {log_dir, "priv/log"},
		 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
	   {webmachine_mochiweb, start, [WebConfig]},
	   permanent, 5000, worker, dynamic},

    Uuids = {uuids,
	     {uuids, start, []},
	     permanent, 5000, worker, dynamic},
    HttpSessionHooks = {hooks,
			{hooks, start_link,  [http_session_hooks]},
			permanent, 5000, worker, dynamic},
    HttpSessionServer = {http_session_server,
			 {http_session_server, start_link, []},
			 permanent, 5000, worker, dynamic},
    AuthServer = {auth_server,
		  {auth_server, start_link, []},
		  permanent, 5000, worker, dynamic},

    SeqServer = {sequence_server,
		 {sequence_server, start_link, []},
		 permanent, 5000, worker, dynamic},

    AuthInternal = {auth_internal,
		    {auth_internal, start_link, []},
		    permanent, 5000, worker, dynamic},

    Processes = [Uuids,
		 SeqServer,
		 HttpSessionHooks, 
		 HttpSessionServer,
		 AuthServer,
		 AuthInternal,
		 Web],
    {ok, {{one_for_one, 10, 10}, Processes}}.
