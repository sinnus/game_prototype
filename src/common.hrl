-record(http_context, {ssid, session_pid}).

%% Logger macros
-define(PRINT(Format, Args),
	io:format(Format, Args)).

-define(DEBUG(Format, Args),	
	error_logger:info_msg(Format, Args)).

%% -define(DEBUG(Format, Args),	
%% 	ok).

-define(INFO_MSG(Format, Args),
	error_logger:info_msg(Format, Args)).
                              
-define(WARNING_MSG(Format, Args),
	error_logger:warning_msg(Format, Args)).
                              
-define(ERROR_MSG(Format, Args),
	error_logger:error_msg(Format, Args)).

-define(CRITICAL_MSG(Format, Args),
	error_logger:error_msg(Format, Args)).
