-module(auth_account).

-export([start/0]).

-record(account, {id,
		  account_ref_id, % Reference to external account storage
		  type,           % type (internal, vkontakte, facebook)
		  enabled}).      % true / false

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    mnesia:create_table(account, [{disc_copies, []},
				  {attributes, record_info(fields, account)},
				  {index, [account_ref_id, type]}]).
