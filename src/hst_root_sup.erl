%% Copyright
-module(hst_root_sup).
-author("jvalduvieco").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
	HackmanClientStartSpec = {{local, hackman_client},
		{hackman_client, start_link, []},
		permanent, infinity, worker, [hackman_client]},
	{ok, {{one_for_one, 1, 1}, [HackmanClientStartSpec]}}.
