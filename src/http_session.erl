%%%-------------------------------------------------------------------
%%% File    : http_session.erl
%%% Author  : sinnus <sinnus@desktop>
%%% Description : Http session. Holds variables, auth and etc...
%%%
%%% Created : 29 Mar 2010 by sinnus <sinnus@desktop>
%%%-------------------------------------------------------------------
-module(http_session).
-include("common.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, stop/1,
	 keepalive/1, check_expire/2,
	 terminate/2, code_change/3]).

-record(session, {
	  expire,
	  timeout
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(SessionExpireTimeout) ->
    gen_server:start_link(?MODULE, [SessionExpireTimeout], []).

stop(Pid) ->
    try
        gen_server:cast(Pid, stop)
    catch _Class:_Term -> 
        error 
    end.

%% @doc Reset the expire counter of the session, called from the page process when comet attaches
keepalive(Pid) ->
    gen_server:cast(Pid, keepalive).

%% @spec check_expire(Now::integer(), Pid::pid()) -> none()
%% @doc Check session and page expiration, periodically called by the session manager
check_expire(Now, Pid) ->
    gen_server:cast(Pid, {check_expire, Now}).

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
init([SessionExpireTimeout]) ->
    Session = #session{
      expire = http_session_manager:now() + SessionExpireTimeout,
      timeout = SessionExpireTimeout
     },
    {ok, Session}.

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

handle_cast(stop, State) ->
    {stop, normal, State};

%% @doc Reset the timeout counter for the session and, optionally, a specific page
handle_cast(keepalive, Session) ->
    Now      = http_session_manager:now(),
    Session1 = Session#session{expire=Now + Session#session.timeout},
    {noreply, Session1};

%% @doc Check session expiration, stop when passed expiration.
handle_cast({check_expire, Now}, Session) ->
    if 
	Session#session.expire < Now -> 
	    {stop, normal, Session};
	true -> 
	    {noreply, Session}
    end;

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
