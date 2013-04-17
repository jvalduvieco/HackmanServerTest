%% Copyright
-module(hst_messages_composer).

%% API
-export([login/1, new_player/1, join_match/1, new_match/1, get_players/1, get_objects/1, position_update/3, pick_object/3]).
-record(client_state, {
	session_id = none,
	player_id = none,
	match_id = none
}).

login(_State) ->
	[{<<"type">>, <<"login">>}].
new_player(State) ->
	[{<<"type">>, <<"newPlayer">>},
		{<<"sessionId">>, State#client_state.session_id},
		{<<"playerId">>, State#client_state.player_id}].
join_match(State) ->
	[{<<"type">>,<<"joinMatch">>},
		{<<"sessionId">>, State#client_state.session_id},
		{<<"data">>}, [{<<"matchId">>, State#client_state.match_id}]].
new_match(State) ->
	[{<<"type">>,<<"newMatch">>},
		{<<"sessionId">>, State#client_state.session_id}].
get_players(State) ->
	[{<<"type">>,<<"getPlayers">>},
		{<<"sessionId">>, State#client_state.session_id}].
get_objects(State) ->
	[{<<"type">>, <<"getObjects">>},
		{<<"sessionId">>, State#client_state.session_id}].
position_update({VX, VY}, {X, Y}, State)->
	[{<<"type">>, <<"positionUpdate">>},
		{<<"sessionId">>, State#client_state.session_id},
		{<<"playerId">>, State#client_state.player_id},
		{<<"pos">>, [{<<"x">>,X},{<<"y">>, Y}]},
		{<<"vel">>, [{<<"x">>,VX},{<<"y">>, VY}]}].
pick_object(ObjectId, ObjectType, State)->
	[{<<"type">>, <<"positionUpdate">>},
		{<<"sessionId">>, State#client_state.session_id},
		{<<"objectId">>, ObjectId},
		{<<"objectType">>, ObjectType}].