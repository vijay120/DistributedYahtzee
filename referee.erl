-module(referee).
-export([main/1]).
-define(DIESTATE, 6).
-define(SCORECARDROWS, 14).
-define(NUMPOSSIBLEDIEOUTCOMES, 15).

main(Params) ->
	_ = os:cmd("epmd -daemon"),
	Reg_name = hd(Params),
	net_kernel:start([list_to_atom(Reg_name), shortnames]),
	io:format("My node is ~p", [node()]),
	io:format("My pid is ~p", [self()]),
	register(referee, self()),
	findMyPlayersAndGameId().


findMyPlayersAndGameId() ->
	receive
		{Pid, assign_players_and_gameId, PlayerAPid, PlayerBPid, GameId, TournamentId} -> 
			io:format("PlayerAPid is ~p", [PlayerAPid]),
			io:format("PlayerBPid is ~p", [PlayerBPid]),
			io:format("GameID is ~p", [GameId]),
			initiateGame(PlayerAPid, PlayerBPid, GameId, TournamentId);
		_ -> io:format("whateves man")
	end.

initiateGame(PlayerAPid, PlayerBPid, GameId, TournamentId) ->
	random:seed(now()),
	timer:sleep(100),
	ScorecardA = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
	DieOutcomesA = generate_fixed_length_lists("random", ?NUMPOSSIBLEDIEOUTCOMES),
	ScorecardB = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
	DieOutcomesB = generate_fixed_length_lists("random", ?NUMPOSSIBLEDIEOUTCOMES),
	io:format("A score: ~p", [ScorecardA]),
	io:format("A die outcomes: ~p", [DieOutcomesA]),
	io:format("B score: ~p", [ScorecardB]),
	io:format("B ie outcomes: ~p", [DieOutcomesA]).


generate_fixed_length_lists(Type, Count) ->
	timer:sleep(100),
	if Count == 0 -> [];
		true -> 	
			if Type == "scorecard" -> [-1] ++ generate_fixed_length_lists(Type, Count-1);
				true -> [element(1, random:uniform_s(?DIESTATE, random:seed(now())))] ++ generate_fixed_length_lists(Type, Count-1)
		end
	end.
