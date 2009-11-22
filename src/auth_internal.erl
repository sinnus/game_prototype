-module(auth_internal).

-export([start/0, check_password/2]).

-record(account, {login,
		  password}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    mnesia:create_table(account, [{disc_copies, []},
				  {attributes, record_info(fields, account)}]).

check_password(Login, Password) ->
    true.
