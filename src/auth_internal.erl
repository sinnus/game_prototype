-module(auth_internal).

-behaviour(gen_server).
-export([start_link/0, authorize/2, register_account/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(internal_account, {id,
			   login,     % login
			   password}).% password  md5 sum
-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    mnesia:create_table(internal_account, [{disc_copies, [node()]},
					   {attributes, record_info(fields, internal_account)},
					   {index, [login]}]),
    {ok, #state{}}.

authorize(Login, Password) ->
    gen_server:call(?MODULE, {authorize, Login, Password}).

register_account(Login, Password) ->
    gen_server:call(?MODULE, {register_account, Login, Password}).

%%%----------------------------------------------------------------------

%% Result: {ok, Id}
%%         {error, absent}
handle_call({authorize, Login, Password}, _From, State) ->
    F = fun() ->
		case mnesia:index_read(internal_account,
				       Login,
				       #internal_account.login) of
		    [] ->
			{error, absent};
		    [InternalAccount] ->
			case InternalAccount#internal_account.password of
			    Password ->
				{ok, InternalAccount#internal_account.id};
			    _ ->
				{error, absent}
			end;
		    _ ->
			{error, absent}
		end
	end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State};

%% Result: {ok, Id}
%%         {error, exists}
handle_call({register_account, Login, Password}, _From, State) ->
    F = fun() ->
		%% Maybe we should use dirty_index_read???
		case mnesia:index_read(internal_account,
				       Login, #internal_account.login) of
		    [] ->
			{ok, create_account(Login, Password)};
		    [_] ->
			{error, exists}
		end
	end,
    {atomic, Result} = mnesia:transaction(F),
    {reply, Result, State}.

create_account(Login, Password) ->
    Seq = sequence_server:get_sequence(internal_account),
    F = fun() -> mnesia:write(#internal_account{id = Seq,
						login = Login,
						password = Password}),
		 auth_db:try_create_account(Seq, 0)
	end,
    {atomic, _Result} = mnesia:transaction(F),
    Seq.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
