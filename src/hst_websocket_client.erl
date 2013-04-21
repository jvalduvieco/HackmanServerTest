-module(hst_websocket_client).

-behaviour(websocket_client_handler).

-export([start_link/1, init/2, websocket_handle/3, websocket_info/3, websocket_terminate/3, send_message/2]).

-record(state, {hackman_client_handle = none}).

start_link(HackmanClientHandle) ->
	lager:info("Starting websocket client..."),
	websocket_client:start_link(hst_config:get(server_url), ?MODULE, HackmanClientHandle).

init(HackmanClientHandle, _ConnState) ->
	hackman_client:send_event(HackmanClientHandle, {<<"connected">>, none},[]),
	{ok, #state{hackman_client_handle = HackmanClientHandle}}.

websocket_handle({pong, _Msg}, _ConnState, State) ->
	{ok, State};
websocket_handle({text, Message}, _ConnState, State) ->
	%lager:debug("==> ~p", [Message]),
	Decoded = jsx:decode(Message),
	{ok, Type} = get_metadata(Decoded),
	Event = {Type, get_data(Decoded)},
	ok = hackman_client:send_event(State#state.hackman_client_handle, Event, State),
	{ok, State}.

websocket_info(Message, _ConnState, State) ->
	%lager:debug("<== ~p",[Message]),
	{reply, {text, jsx:encode(Message)}, State}.

websocket_terminate({close, Code, Payload}, _ConnState, State) ->
	lager:warning("Websocket closed in state ~p wih code ~p and payload ~p",
		[State, Code, Payload]),
	ok.

send_message(WebsocketClientHandle, Message) ->
	WebsocketClientHandle ! Message.

%% Tools
get_metadata(Message) ->
	{ok, proplists:get_value(<<"type">>, Message,none)}.
get_data(Message) ->
	proplists:get_value(<<"data">>, Message, none).