-module(referee).
-export([main/1, send_die_from_choice/5]).
-define(DIESTATE, 6).
-define(SCORECARDROWS, 13).
-define(NUMPOSSIBLEDIEOUTCOMES, 15).
-define(NEXTDIE, 5).
-define(STARTINDEX, 1).
-define(FIRSTROUND, 1).

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
	ChoiceA = [true, true, true, true, true],
	ChoiceB = [true, true, true, true, true],
	handle_round(TournamentId, GameId, ?FIRSTROUND, ScorecardA, ScorecardB, DieOutcomesA, DieOutcomesB, PlayerAPid, PlayerBPid, ChoiceA, ChoiceB).


 handle_round(Tid, Gid, Round, PlayerAScore, PlayerBScore, DieOutcomesA, DieOutcomesB, PlayerAPid, PlayerBPid, ChoiceA, ChoiceB) ->

 	%Step 1: Calculate the dies that need to be send for each player
 	DieToA = send_die_from_choice(DieOutcomesA, ChoiceA, Round, ?STARTINDEX, []),
 	%Step 2: Send the message!
 	PlayerAPid ! {make_ref(), Tid, Gid, Round, DieToA, PlayerAScore, PlayerBScore},

 	DieToB = send_die_from_choice(DieOutcomesB, ChoiceB, Round, ?STARTINDEX, []),
 	PlayerAPid ! {make_ref(), Tid, Gid, Round, DieToB, PlayerBScore, PlayerAScore},

 	receive
 		{Ref, Tid, Gid, roll-number, dice-to-keep, scorecard-line }




%this method is used to find the next roll to die to pass on to the players
send_die_from_choice(DieSequence, Choice, Round, CurrentIndex, AccumulatedDieSeq) ->
	if  CurrentIndex > length(Choice) -> AccumulatedDieSeq;
	true -> 
		Boolean = lists:nth(CurrentIndex, Choice),
		if Boolean == false -> 
			NewAccumulatedDieSeq = AccumulatedDieSeq ++ [lists:nth(CurrentIndex*Round + ?NEXTDIE, DieSequence)],
			send_die_from_choice(DieSequence, Choice, Round, CurrentIndex+1, NewAccumulatedDieSeq);
		true -> 
			NewAccumulatedDieSeq = AccumulatedDieSeq ++ [lists:nth(CurrentIndex, DieSequence)],
			send_die_from_choice(DieSequence, Choice, Round, CurrentIndex+1, NewAccumulatedDieSeq)
		end
	end.


generate_fixed_length_lists(Type, Count) ->
	timer:sleep(10),
	if Type == "scorecard" ->
		if Count == 0 -> [0]; %this is the bonus score
			true -> [-1] ++ generate_fixed_length_lists(Type, Count-1)
		end;
	true -> 
		if Count == 0 -> [];
			true -> [element(1, random:uniform_s(?DIESTATE, random:seed(now())))] ++ generate_fixed_length_lists(Type, Count-1)
		end
	end.

=======
%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(referee).

-import(distributed_yahtzee, [println/1, println/2]).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([referee_main/1]).
%% ====================================================================
%%                             Constants
%% ====================================================================
-define(GLOBALNAME, "Referee").
-define(DIESTATE, 6).
-define(SCORECARDROWS, 14).
-define(NUMPOSSIBLEDIEOUTCOMES, 15).

%% ====================================================================
%%                            Main Function
%% ====================================================================
referee_main(Params) ->
  os:cmd("epmd -daemon"),
  Reg_name = hd(Params),
  net_kernel:start([list_to_atom(Reg_name), shortnames]),
  printnameln("My node is ~p", [node()]),
  printnameln("My pid is ~p", [self()]),
  register(referee, self()),
  findMyPlayersAndGameId().

getName() ->
  ?GLOBALNAME ++ io_lib:format("~p", [self()]).

findMyPlayersAndGameId() ->
  receive
    {Pid, assign_players_and_gameId, PlayerAPid, PlayerBPid, GameId, TournamentId} -> 
      printnameln("PlayerAPid is ~p", [getName() ,PlayerAPid]),
      printnameln("PlayerBPid is ~p", [PlayerBPid]),
      printnameln("GameID is ~p", [GameId]),
      initiateGame(PlayerAPid, PlayerBPid, GameId, TournamentId);
    _ -> printnameln("whateves man", [getName()])
  end.

initiateGame(PlayerAPid, PlayerBPid, GameId, TournamentId) ->
  random:seed(now()),
  timer:sleep(100),
  ScorecardA = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
  DieOutcomesA = generate_fixed_length_lists("random", ?NUMPOSSIBLEDIEOUTCOMES),
  ScorecardB = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
  DieOutcomesB = generate_fixed_length_lists("random", ?NUMPOSSIBLEDIEOUTCOMES),
  printnameln("A score: ~p", [ScorecardA]),
  printnameln("A die outcomes: ~p", [DieOutcomesA]),
  printnameln("B score: ~p", [ScorecardB]),
  printnameln("B ie outcomes: ~p", [DieOutcomesA]).


generate_fixed_length_lists(Type, Count) ->
  timer:sleep(100),
  if Count == 0 -> [];
    true ->   
      if Type == "scorecard" -> [-1] ++ generate_fixed_length_lists(Type, Count-1);
        true -> [element(1, random:uniform_s(?DIESTATE, random:seed(now())))] ++ generate_fixed_length_lists(Type, Count-1)
    end
  end.

printnameln(ToPrint) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint).

printnameln(ToPrint, Options) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint, Options).
