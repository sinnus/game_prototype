-module(auth_db).

-export([start/0, try_create_account/2]).

-record(account, {id,
		  account_ref_id, % Reference to external account storage
		  type,           % type (internal, vkontakte, facebook)
		  enabled}).      % true / false

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    mnesia:create_table(account, [{disc_copies, [node()]},
				  {attributes, record_info(fields, account)},
				  {index, [account_ref_id, type]}]).

%% Must be synchronized by type using gen_server
%% Result: created
%%         exists
try_create_account(AccountRefId, Type) ->
    F = fun() ->
		case mnesia:index_read(account, AccountRefId, #account.account_ref_id) of
		    [] ->
			create_account(AccountRefId, Type);
		    [Accounts] ->
			case lists:filter(fun(Elem)  -> 
						  Elem#account.type == Type
					  end, Accounts) of
			    [] ->
				create_account(AccountRefId, Type);
			    [_] ->
				%% TODO: log warning if length(Accounts) > 1
				exists
			end
		end
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

create_account(_AccountRefId, _Type) ->
    created.
