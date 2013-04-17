%% Copyright
-module(hackman_client).
-author("jvalduvieco").

-behaviour(gen_fsm).

%% API
-export([start_link/0, send_event/3]).

%% gen_fsm
-export([init/1, waiting_connect/2, waiting_login/2, handle_event/3, state_name/3,
	handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-record(state, {websocket_client_pid = none}).
%% API
start_link() ->
	lager:debug("HackmanClient starting..."),
	gen_fsm:start_link(?MODULE, [], []).

send_event(HackmanClientHandle, Message, _State) ->
	lager:debug("ref: ~p ~p",[HackmanClientHandle, Message]),
	gen_fsm:send_event(HackmanClientHandle, Message).

%% gen_fsm callbacks
init(_Args) ->
	lager:debug("HackmanClient started..."),
	{ok, WebsocketClientPid} = hst_websocket_client:start_link(erlang:self()),
	{ok, login, #state{websocket_client_pid = WebsocketClientPid}}.
waiting_connect({connected}, State) ->
	hst_websocket_client:send_message(State#state.websocket_client_pid, hst_messages_composer:login()),
	{next_state, waiting_login, State}.
waiting_login({loginResponse, _Data}, State) ->
	lager:debug("Login!"),
	{next_state, state_name, State}.

state_name(_Event, _From, State) ->
	{reply, ok, state_name, State}.

handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	{reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.
