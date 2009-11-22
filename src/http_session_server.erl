%%%-------------------------------------------------------------------
%%% File    : http_session_server.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description : 
%%%
%%% Created : 26 Oct 2009 by sinnus <sinnus@linux>
%%%-------------------------------------------------------------------
-module(http_session_server).
-include("common.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, stop/1, new_session/1, check_session/2,
	 remove_session/2, update_session/2, new_or_update_session/2,
	 fire_remove_session_event/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SESSION_TIMEOUT, 1*60). %% Session timeout in secs

-record(http_session, 
	{ssid,               %% session id (cookie ssid). binary type
	 start_time,         %% start time in secs (utc)
	 last_access_time,   %% last access time in secs (utc)
	 tref}).        %% timeout timer pid

-record(state,
	{ssid_set}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(ServerLink) ->
    gen_server:cast(ServerLink, stop).

%% Fire new_session_event
new_session(ServerLink) ->
    SessionId = gen_server:call(ServerLink, {new_session}, infinity),
    fire_new_session_event(SessionId),
    SessionId.

check_session(ServerLink, SessionId) ->
    gen_server:call(ServerLink, {check_session, SessionId}, infinity).

%% Fire remove_session_event
remove_session(ServerLink, SessionId) ->
    Result = gen_server:call(ServerLink, {remove_session, SessionId}, infinity),
    case Result of
	no_session ->
	    Result;
	ok ->
	    fire_remove_session_event(SessionId),
	    Result
    end.

update_session(ServerLink, SessionId) ->
    gen_server:call(ServerLink, {update_session, SessionId}, infinity).    

%% Fire new_session_event if it created
new_or_update_session(ServerLink, SessionId) ->
    Result = gen_server:call(ServerLink, {new_or_update_session, SessionId}, infinity),
    case Result of
	{new, NewSessionId} ->
	    fire_new_session_event(NewSessionId),
	    Result;
	_ ->
	    Result
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{
       ssid_set = ets:new(?MODULE, [set, private, {keypos, 2}])
      }}.

%% New ssid
handle_call({new_session}, _From, State) ->
    SessionId = gen_uuid(),
    Now = gnow_utc(),
    TRef = do_start_timer(SessionId),
    HttpSession = #http_session{
      ssid = SessionId,
      start_time = Now,
      last_access_time = Now,
      tref = TRef
     },
    ets:insert(State#state.ssid_set, HttpSession),
    {reply, SessionId, State};

%%  Check valid ssid
handle_call({check_session, SessionId}, _From, State) ->
    case do_check_session(SessionId, State) of
	true ->
	    {reply, true, State};
	false ->
	    {reply, false, State}
    end;

%% Remove ssid
%% FIXME: Result session_expired if no session
handle_call({remove_session, SessionId}, _From, State) ->
    Reply = do_remove_session(State#state.ssid_set, SessionId),
    {reply, Reply, State};

%% Update session by ssid
%% FIXME: Result session_expired if no session
handle_call({update_session, SessionId}, _From, State) ->
    case ets:lookup(State#state.ssid_set, SessionId) of
	[] ->
	    {reply, ok, State};
	[HttpSessionRow] ->
	    timer:cancel(HttpSessionRow#http_session.tref),
	    TRef = do_start_timer(SessionId),
	    Now = gnow_utc(),
	    HttpSession = HttpSessionRow#http_session{last_access_time = Now,
						      tref = TRef},
	    ets:insert(State#state.ssid_set, HttpSession),
	    {reply, ok, State}
    end;

%% Create new http session or update it if one exists
%% Result - tuple {new, SessionId} | {update, SessionId}
handle_call({new_or_update_session, SessionId}, _From, State) ->
    case do_check_session(SessionId, State) of
	true ->
	    {reply, ok, NewState} =  handle_call({update_session, SessionId}, _From, State),
	    Reply = {update, SessionId};
	false ->
	    {reply, NewSessionId,  NewState} = handle_call({new_session}, _From, State),
	    Reply = {new, NewSessionId}
    end,
    {reply, Reply, NewState}.

%% Stop Server
handle_cast(stop, State) ->
    {stop, normal, State};

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({remove_expired_session, SessionId}, State) ->
    do_remove_expired_session(SessionId, State),
    {noreply, State};

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
gnow_utc() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:universal_time()).

gen_uuid() ->
    uuids:new().

do_check_session(SessionId, State) ->
    HttpSessionRow = ets:lookup(State#state.ssid_set, SessionId),
    case HttpSessionRow of
	[] ->
	    false;
	_ ->
	    true
    end.

%% Cancel timer and remove ssid from storage
do_remove_session(Tid, SessionId) ->
    case ets:lookup(Tid, SessionId) of
	[] ->
	    no_session;
	[HttpSessionRow] ->
	    timer:cancel(HttpSessionRow#http_session.tref),
	    ets:delete(Tid, SessionId),
	    ok
    end.

do_start_timer(SessionId) ->
    {ok, TRef} = timer:send_after(?SESSION_TIMEOUT * 1000,
				  {remove_expired_session, SessionId}),
    TRef.

do_remove_expired_session(SessionId, State) ->
    case ets:lookup(State#state.ssid_set, SessionId) of
	[] ->
	    no_session;
	[HttpSessionRow] ->
	    Now = gnow_utc(),
	    if
		(HttpSessionRow#http_session.last_access_time + ?SESSION_TIMEOUT) =< Now ->
		    ?DEBUG("Timeout occurred for SesionId ~p", [SessionId]),
		    Result = do_remove_session(State#state.ssid_set, SessionId),
		    case Result of
			ok ->
			    spawn(?MODULE, fire_remove_session_event, [SessionId]),
			    Result;
			no_session ->
			    Result
		    end
	    end
    end.

fire_new_session_event(SessionId) ->
    ?DEBUG("New_session_event ~p", [SessionId]),
    hooks:run(http_session_hooks, new_session, [SessionId]),
    ok.

fire_remove_session_event(SessionId) ->
    ?DEBUG("Remove_session_event ~p", [SessionId]),
    hooks:run(http_session_hooks, remove_session, [SessionId]),
    ok.
