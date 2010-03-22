-module(auth_internal).

-export([start/0, authorize/2, register_account/2]).

-record(internal_account, {id,
			   login,     % login
			   password}).% password  md5 sum

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    mnesia:create_table(internal_account, [{disc_copies, [node()]},
					   {attributes, record_info(fields, internal_account)},
					   {index, [login]}]).

%% Should be atomic
%% Result: {ok, Id}
%%         {error, absent}
authorize(Login, Password) ->
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
    Result.

%% Should be atomic
%% Result: {ok, Id}
%%         {error, exists}
register_account(Login, Password) ->
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
    Result.

create_account(Login, Password) ->
    Seq = sequence_server:get_sequence(internal_account),
    F = fun() -> mnesia:write(#internal_account{id = Seq,
						login = Login,
						password = Password}),
		 auth_db:try_create_account(Seq, 0)
	end,
    {atomic, _Result} = mnesia:transaction(F),
    Seq.
