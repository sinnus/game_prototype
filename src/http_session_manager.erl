%%%-------------------------------------------------------------------
%%% File    : http_session_manager.erl
%%% Author  : sinnus <sinnus@desktop>
%%% Description : 
%%%
%%% Created : 29 Mar 2010 by sinnus <sinnus@desktop>
%%%-------------------------------------------------------------------
-module(http_session_manager).
-include("common.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, stop/1,
	 ensure_session/1,
	 terminate/2, code_change/3]).

-record(sessions, {sid2pid,
		   pid2sid}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

ensure_session(Context) ->
    case gen_server:call(?MODULE, {ensure_session, true, Context}) of
        {ok, Context1} -> Context1;
        {error, _} -> Context
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    State = #sessions{sid2pid=dict:new(), pid2sid=dict:new()},
    %% TODO: ???
    %%process_flag(trap_exit, true),
    {ok, State}.

%% Ensure session
handle_call({ensure_session, AllowNew, Context}, _From, State) ->
    SessionId = Context#http_context.ssid,
    case session_find_pid(SessionId, State) of
	Pid when is_pid(Pid) orelse AllowNew ->
	    {Context1, State1} = ensure_session1(State, Pid, Context, State),
	    {reply, {ok, Context1}, State1};
	undefined ->
	    {reply, {error, no_session_pid}, State}
    end;

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Make sure that the session cookie is set and that the session process has been started.
ensure_session1(S, P, Context, State) when S == undefined orelse P == error ->
    Pid       = spawn_session(State),
    SessionId = uuids:new(),
    State1    = store_session_pid(SessionId, Pid, State),
    Context1  = Context#http_context{ssid=SessionId},
    {Context1, State1};
ensure_session1(_SessionId, _Pid, Context, _State) ->
    %%z_session:keepalive(Context#context.page_pid, Pid),
    %%Context1  = Context#context{session_pid = Pid},
    {Context, _State}.

%% @spec session_find_pid(string(), State) ->  error | pid()
%% @doc find the pid associated with the session id
session_find_pid(undefined, _State) ->
    error;
session_find_pid(SessionId, State) ->
    case dict:find(SessionId, State#sessions.sid2pid) of
        {ok, Pid} ->
            Pid;
        error ->
            error
    end.

%% @spec store_session_pid(pid(), State) -> State
%% @doc Add the pid to the session state
store_session_pid(SessionId, Pid, State) ->
    State#sessions{
      pid2sid = dict:store(Pid, SessionId, State#sessions.pid2sid),
      sid2pid = dict:store(SessionId, Pid, State#sessions.sid2pid)
     }.

spawn_session(_State) ->
    case http_session:start_link() of
        {ok, Pid} ->
	    erlang:monitor(process, Pid),
	    Pid
    end.
