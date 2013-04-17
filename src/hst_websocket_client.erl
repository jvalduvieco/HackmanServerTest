-module(hst_websocket_client).

-behaviour(websocket_client_handler).

-export([start_link/1, init/2, websocket_handle/3, websocket_info/3, websocket_terminate/3, send_message/2]).

-record(state, {hackman_client_handle = none}).

start_link(HackmanClientHandle) ->
	lager:debug("Starting websocket client..."),
	websocket_client:start_link(hst_config:get(server_url), ?MODULE, [HackmanClientHandle]).

init(HackmanClientHandle, ConnState) ->
	lager:debug("Connected! ~p",[ConnState]),
	hackman_client:send_event(HackmanClientHandle, {connected},[]),
	{ok, [#state{hackman_client_handle = HackmanClientHandle}]}.

websocket_handle({pong, _Msg}, _ConnState, State) ->
	{ok, State};
websocket_handle(Message, _ConnState, State) ->
	io:format("Received msg ~p~n", [Message]),
	Decoded = jsx:decode(Message),
	%%lager:debug("Decoded: ~p",[Decoded]),
	{Type, _Session} = get_metadata(Decoded),
	Event = {Type, get_data(Decoded)},
	{Action, Response, NewState} = hackman_client:send_event(State#state.hackman_client_handle, Event, State),
	handle_result(Action, Response, NewState).



websocket_info(Message, _ConnState, State) ->
	{reply, {text, jsx:encode(Message)}, State}.

websocket_terminate({close, Code, Payload}, _ConnState, State) ->
	io:format("Websocket closed in state ~p wih code ~p and payload ~p~n",
		[State, Code, Payload]),
	ok.

send_message(WebsocketClientHandle, Message) ->
	WebsocketClientHandle ! Message.

%% Tools
get_metadata(Message) ->
	{proplists:get_value(<<"type">>, Message,none),
		proplists:get_value(<<"sessionId">>, Message, none)}.
get_data(Message) ->
	proplists:get_value(<<"data">>, Message, none).
handle_result(reply, Data, State) ->
%lager:debug("translate_to_websocket_resoponse: ~p",Data),
	{reply, {text, jsx:encode(Data)}, State};
handle_result(noreply, _Data, State) ->
	{ok, State};
handle_result(_Action, Response, State) ->
	lager:error("Unhandled response: ~p", [Response]),
	{ok, State}.