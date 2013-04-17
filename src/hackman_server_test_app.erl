%% Copyright
-module(hackman_server_test_app).
-author("jvalduvieco").

-behaviour(application).

% application
-export([start/2, stop/1]).

% application callbacks
start(_Type, _Args) ->
	%lager:set_loglevel(lager_console_backend, hst_config:get(log_level)),

	lager:set_loglevel(lager_console_backend, debug),
	lager:debug("wwwwwwwww"),
	hst_root_sup:start_link().

stop(_State) ->
	ok.
