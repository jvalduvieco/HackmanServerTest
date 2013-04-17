%% Copyright
-module(hst_config).
-author("jvalduvieco").

%% API
-export([get/1, get/2]).
-define(APPLICATION, hackman_server_test).


get(Key) ->
	application:get_env(?APPLICATION, Key, none).
get(Key, Default) ->
	application:get_env(?APPLICATION, Key, Default).