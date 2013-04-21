-module(hackman_client).

-include("client_state.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/0, send_event/3]).

%% gen_fsm
-export([init/1, waiting_connect/2, waiting_login/2, handle_event/3, state_name/3, waiting_create_match/2, waiting_list_matches/2, waiting_join/2, waiting_new_player/2,
	waiting_get_objects/2, waiting_start_match/2, playing/2,
	handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(HS_CLIENT_MOVE_TIME, 1000).
-define(HS_CLIENT_SPEED, 3).

-record(state, {websocket_client_pid = none}).
%% API
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

send_event(HackmanClientHandle, Message = {<<"newPlayerAnnounce">>, _Data}, _State) ->
	gen_fsm:send_all_state_event(HackmanClientHandle, Message);
send_event(HackmanClientHandle, Message = {<<"positionUpdateAnnounce">>, _Data}, _State) ->
	gen_fsm:send_all_state_event(HackmanClientHandle, Message);
send_event(HackmanClientHandle, Message = {<<"endMatch">>, _Data}, _State) ->
	gen_fsm:send_all_state_event(HackmanClientHandle, Message);
send_event(HackmanClientHandle, Message, _State) ->
	gen_fsm:send_event(HackmanClientHandle, Message).

%% gen_fsm callbacks
init(_Params) ->
	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed({A,B,C}),
	GameId = hst_config:get(game_id, none),
	case hst_websocket_client:start_link(erlang:self()) of
		{ok, WebsocketClientPid} ->
			{ok, waiting_connect, #client_state{websocket_client_pid = WebsocketClientPid, game_id = GameId}};
		{error, Reason} ->
			{stop, {error,Reason}}
		end.
waiting_connect({<<"connected">>, _Data}, State) ->
	hst_websocket_client:send_message(State#client_state.websocket_client_pid, hst_messages_composer:login(State)),
	{next_state, waiting_login, State}.
waiting_login({<<"loginResponse">>, Data}, State) ->
	Session = proplists:get_value(<<"sessionId">>, Data, none),
	NewState = State#client_state{session_id = Session, player_id = <<"robot">>},
	case choose_one([join_match, create_match]) of
		join_match ->
			hst_websocket_client:send_message(
				NewState#client_state.websocket_client_pid, hst_messages_composer:list_matches(NewState)),
			{next_state, waiting_list_matches, NewState};
		create_match ->
			hst_websocket_client:send_message(
				NewState#client_state.websocket_client_pid, hst_messages_composer:new_match(NewState)),
			{next_state, waiting_create_match, NewState}
	end.
waiting_create_match ({<<"newMatchResponse">>, _Data}, State) ->
	hst_websocket_client:send_message(State#client_state.websocket_client_pid, hst_messages_composer:new_player(State)),
	{next_state, waiting_new_player, State}.
waiting_list_matches ({<<"listMatchesResponse">>, Data}, State) ->
	MatchList = proplists:get_value(<<"matches">>, Data, none),
	case MatchList of
		[] ->
			%% No matches available
			hst_websocket_client:send_message(State#client_state.websocket_client_pid, hst_messages_composer:new_match(State)),
			{next_state, waiting_create_match, State};
		_ ->
			% Pick a random match
			{MatchId, _} = choose_one(MatchList),
			TmpState = State#client_state{match_id = MatchId},
			hst_websocket_client:send_message(
				State#client_state.websocket_client_pid, hst_messages_composer:join_match(TmpState)),
			{next_state, waiting_join, TmpState}
	end.
waiting_join({<<"joinMatchResponse">>, _Data}, State) ->
	hst_websocket_client:send_message(State#client_state.websocket_client_pid, hst_messages_composer:new_player(State)),
	{next_state, waiting_new_player, State}.
waiting_new_player ({<<"newPlayerResponse">>, _Data}, State) ->
	hst_websocket_client:send_message(State#client_state.websocket_client_pid, hst_messages_composer:get_objects(State)),
	{next_state, waiting_get_objects, State}.
waiting_get_objects({<<"getObjectsResponse">>, _Data}, State) ->
	{next_state, waiting_start_match, State}.
waiting_start_match({<<"startMatch">>, _Data}, State) ->
	{X, Y} = {State#client_state.x, State#client_state.y},
	{Vx, Vy} = {State#client_state.vx, State#client_state.vy},
	Message = hst_messages_composer:position_update({X, Y}, {Vx, Vy}, State),
	hst_websocket_client:send_message(State#client_state.websocket_client_pid, Message),
	% move timer
	MoveTimer = create_move_timer(),
	{next_state, playing, State#client_state{move_timer = MoveTimer}}.
playing({timeout, _Ref, move_timeout}, State) ->
	NewState = State#client_state{x = State#client_state.x + ?HS_CLIENT_SPEED, y = State#client_state.y + ?HS_CLIENT_SPEED},
	{X, Y} = {NewState#client_state.x, NewState#client_state.y},
	{Vx, Vy} = {NewState#client_state.vx, NewState#client_state.vy},
	Message = hst_messages_composer:position_update({X, Y}, {Vx, Vy}, NewState),
	hst_websocket_client:send_message(NewState#client_state.websocket_client_pid, Message),
	MoveTimer = create_move_timer(),
	{next_state, playing, NewState#client_state{move_timer = MoveTimer}};
playing(Event, State) ->
	lager:warning("Unhandled event ~p",[Event]),
	{next_state, playing, State}.

state_name(_Event, _From, State) ->
	{reply, ok, state_name, State}.

handle_event({<<"newPlayerAnnounce">>, _Data}, StateName, State) ->
	{next_state, StateName, State};
handle_event({<<"positionUpdateAnnounce">>, _Data}, StateName, State) ->
	{next_state, StateName, State};
handle_event({<<"endMatch">>, _Data}, _StateName, State) ->
	cancel_move_timer(State#client_state.move_timer),
	hst_websocket_client:send_message(State#client_state.websocket_client_pid, hst_messages_composer:get_objects(State)),
	{next_state, waiting_get_objects, State}.

handle_sync_event(_Event, _From, StateName, State) ->
	{reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
	{next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
	ok.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

choose_one(Options) ->
	lists:nth(random:uniform(length(Options)), Options).
%% Use choose_one(Option,true) for testing purposes.
choose_one(Options, true) ->
	[Head|_Rest] = Options,
	Head.

create_move_timer() ->
	gen_fsm:start_timer(?HS_CLIENT_MOVE_TIME, move_timeout).
cancel_move_timer(none) ->
	ok;
cancel_move_timer(Ref) ->
	gen_fsm:cancel_timer(Ref).