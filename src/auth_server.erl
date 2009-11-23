%%%-------------------------------------------------------------------
%%% File    : auth_server.erl
%%% Author  : sinnus <sinnus@linux>
%%% Description :  gen_server contains ssid2login mapping
%%%
%%% Created :  2 Nov 2009 by sinnus <sinnus@linux>
%%%-------------------------------------------------------------------
-module(auth_server).

-behaviour(gen_server).

-include("auth_server.hrl").

%% API
-export([start_link/0, register_new_session/3, check_by_ssid/2,
	 get_all_sessions/2, stop/1, remove_session/2, get_login_by_ssid/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,
	{ssid2login_set, %% 1:1
	 login2ssid_set  %% N:N
	}).

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

register_new_session(ServerLink, SessionId, Login) ->
    gen_server:call(ServerLink, {register_new_session, SessionId, Login}, infinity).

check_by_ssid(ServerLink, SessionId) ->
    gen_server:call(ServerLink, {check_by_ssid, SessionId}, infinity).

get_login_by_ssid(ServerLink, Ssid) ->
    gen_server:call(ServerLink, {get_login_by_ssid, Ssid}, infinity).

remove_session(ServerLink, SessionId) ->
    gen_server:call(ServerLink, {remove_session, SessionId}, infinity).

get_all_sessions(ServerLink, Login) ->
    gen_server:call(ServerLink, {get_all_sessions, Login}, infinity).
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
       ssid2login_set = ets:new(?MODULE, [set, private, {keypos, 2}]),
       login2ssid_set = ets:new(?MODULE, [bag, private, {keypos, 3}])
      }}.

handle_call({register_new_session, SessionId, Login}, _From, State) ->
    do_remove_session(SessionId, State),
    AuthInfo = #auth_info {
      ssid = SessionId,
      login = Login
     },
    ets:insert(State#state.ssid2login_set, AuthInfo),
    ets:insert(State#state.login2ssid_set, AuthInfo),
    {reply, ok, State};

handle_call({check_by_ssid, SessionId}, _From, State) ->
    Reply = case ets:lookup(State#state.ssid2login_set, SessionId) of
		[_] ->
		    true;
		[] ->
		    false
	    end,
    {reply, Reply, State};    

handle_call({remove_session, SessionId}, _From, State) ->
    do_remove_session(SessionId, State),
    {reply, ok, State};

handle_call({get_login_by_ssid, Ssid}, _From, State) ->
    Reply = case ets:lookup(State#state.ssid2login_set, Ssid) of
		[Row] ->
		    Row#auth_info.login;
		[] ->
		    undefined
	    end,
    {reply, Reply, State};

%% Can return huge information data
handle_call({get_all_sessions, Login}, _From, State) ->
    Rows = ets:lookup(State#state.login2ssid_set, Login),
    {reply, Rows, State};
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
do_remove_session(SessionId, State) ->
    case ets:lookup(State#state.ssid2login_set, SessionId) of
	[Row] ->
	    ets:delete(State#state.ssid2login_set, SessionId),
	    ets:match_delete(State#state.login2ssid_set, #auth_info{
					   ssid = SessionId,
					   login = Row#auth_info.login
					  });
	[] ->
	    true
    end.
