%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(referee).

-import(distributed_yahtzee, [println/1, println/2]).
-import(yahtzee_player1, [calcUpper/3, calcThreeKind/2, calcFourKind/2,
		 calcFullHouse/2, calcSmallStraight/2, calcLargeStraight/2, calcYahtzee/2,
		 calcChance/2]).

%% ====================================================================
%%                             Public API
%% ====================================================================
-export([referee_main/1]).
%% ====================================================================
%%                             Constants
%% ====================================================================
-define(GLOBALNAME, "Referee").
-define(DIESTATE, 6).
-define(SCORECARDROWS, 13).
-define(NUMPOSSIBLEDIEOUTCOMES, 15).
-define(NEXTDIE, 5).
-define(STARTINDEX, 1).
-define(FIRSTROUND, 1).
-define(FIRSTROLL, 1).
-define(INITIALSCORE, -12).
-define(THREEKIND, 7).
-define(FOURKIND, 8).
-define(FULLHOUSE, 9).
-define(SMALLSTRAIGHT, 10).
-define(LARGESTRAIGHT, 11).
-define(YAHTZEE, 12).
-define(CHANCE, 13).
-define(UPPERCARDS, 7).
-define(INITIALDIECHOICE, [true, true, true, true, true]).
-define(YAHTZEEINDEX, 12).
-define(BONUSINDEX, 14).

%% ====================================================================
%%                            Main Function
%% ====================================================================
referee_main(Params) ->
	_ = os:cmd("epmd -daemon"),
	Reg_name = hd(Params),
	net_kernel:start([list_to_atom(Reg_name), shortnames]),
	io:format("My node is ~p", [node()]),
	io:format("My pid is ~p", [self()]),
	register(referee, self()),
	findMyPlayersAndGameId().


findMyPlayersAndGameId() ->
	receive
		{Pid, assign_players_and_gameId, PlayerAName, PlayerBName, PlayerAPid, PlayerBPid, GameId, TournamentId} -> 
			io:format("PlayerAPid is ~p", [PlayerAPid]),
			io:format("PlayerBPid is ~p", [PlayerBPid]),
			io:format("GameID is ~p", [GameId]),
			io:format("Player A userid is ~p", [PlayerAName]),
			io:format("Player B userid is ~p", [PlayerBName]),
			random:seed(now()),
			timer:sleep(100),
			ScorecardA = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
			ScorecardB = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
			handle_game(?FIRSTROUND, TournamentId, GameId, PlayerAName, PlayerBName, ScorecardA, ScorecardB, PlayerAPid, PlayerBPid);
		_ -> io:format("whateves man")
	end.

%This function handles all the logic and enforcement of rukes
score_logic(ScoreCardChoice, ScoreCardChoiceValue, DiceGiven) ->
	if ScoreCardChoice < ?UPPERCARDS ->
		yahtzee_player1:calcUpper(DiceGiven, ScoreCardChoiceValue, ScoreCardChoice);
		true ->
			case ScoreCardChoice of
				?THREEKIND -> yahtzee_player1:calcThreeKind(DiceGiven, ScoreCardChoiceValue);
				?FOURKIND -> yahtzee_player1:calcFourKind(DiceGiven, ScoreCardChoiceValue);
				?FULLHOUSE -> yahtzee_player1:calcFullHouse(DiceGiven, ScoreCardChoiceValue);
				?SMALLSTRAIGHT -> yahtzee_player1:calcSmallStraight(DiceGiven, ScoreCardChoiceValue);
				?LARGESTRAIGHT -> yahtzee_player1:calcLargeStraight(DiceGiven, ScoreCardChoiceValue);
				?YAHTZEE -> yahtzee_player1:calcYahtzee(DiceGiven, ScoreCardChoiceValue);
				?CHANCE -> yahtzee_player1:calcChance(DiceGiven, ScoreCardChoiceValue)
		end
	end.

handle_game(	Round,
				Tid, 
				Gid, 
				PlayerAName, PlayerBName, 
				PlayerAScoreCard, PlayerBScoreCard, 
				PlayerAPid, PlayerBPid) ->

	if Round > 13 -> 
			io:format("Game is done!");
	true -> 
			random:seed(now()),
			timer:sleep(100),
			DieOutcomesA = generate_fixed_length_lists("random", ?NUMPOSSIBLEDIEOUTCOMES),
			DieOutcomesB = generate_fixed_length_lists("random", ?NUMPOSSIBLEDIEOUTCOMES),
			ChoiceA = ?INITIALDIECHOICE,
			ChoiceB = ?INITIALDIECHOICE,

			[NewPlayerAScoreCard, NewPlayerBScoreCard]	=				handle_roll(	Tid, 
																						Gid, 
																						?FIRSTROLL, 
																						PlayerAName, PlayerBName, 
																						PlayerAScoreCard, PlayerBScoreCard, 
																						DieOutcomesA, DieOutcomesB, 
																						PlayerAPid, PlayerBPid, 
																						ChoiceA, ChoiceB),

			handle_game(	Round+1,
							Tid, 
							Gid, 
							PlayerAName, PlayerBName, 
							NewPlayerAScoreCard, NewPlayerBScoreCard,
							PlayerAPid, PlayerBPid)
	end.

	
handle_roll(	Tid, 
				Gid, 
				Roll, 
				PlayerAName, PlayerBName, 
				PlayerAScoreCard, PlayerBScoreCard, 
				DieOutcomesA, DieOutcomesB,
				PlayerAPid, PlayerBPid, 
				ChoiceA, ChoiceB) ->

	if Roll > 3 ->
		%The last roll is over, so pass the results to the tournament manager
		% io:format("Player A's score is: ~p~n", [PlayerAScore]),
		% io:format("Player B's score is: ~p~n", [PlayerBScore]),
		TotalScoreForA = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerAScoreCard),
		TotalScoreForB = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerBScoreCard),

		if TotalScoreForA > TotalScoreForB -> io:format("Player A wins");
			true -> io:format("Player B wins!")
		end,

		[PlayerAScoreCard, PlayerBScoreCard];


		true ->
			%Step 1: Calculate the dies that need to be send for each player
			DieToA = send_die_from_choice(DieOutcomesA, ChoiceA, Roll, ?STARTINDEX, []),
			DieToB = send_die_from_choice(DieOutcomesB, ChoiceB, Roll, ?STARTINDEX, []),

			ReplacedScoreCardA = checkIfYahtzeeBonusApplicable(DieToA, PlayerAScoreCard),
			ReplacedScoreCardB = checkIfYahtzeeBonusApplicable(DieToB, PlayerBScoreCard),

			%Step 2: Send the message!
			{player, list_to_atom("vijay@wl-194-96")} ! {play_request, self(), PlayerAName, {make_ref(), Tid, Gid, Roll, DieToA, ReplacedScoreCardA, ReplacedScoreCardB}},
			{player, list_to_atom("vijay120@wl-194-96")}  ! {play_request, self(), PlayerBName, {make_ref(), Tid, Gid, Roll, DieToB, ReplacedScoreCardB, ReplacedScoreCardA}},

			%Recieve for player A only
			receive
				{play_action, PidA, PlayerAName, {RefA, TidA, GidA, RollNumberA, DiceToKeepA, ScorecardAChoice}} -> 
					%Receive for player B only
					receive
						{play_action, PidB, PlayerBName, {RefB, TidB, GidB, RollNumberB, DiceToKeepB, ScorecardBChoice}} -> 

							%So do not care about any of the score update logic until roll 3
							if Roll == 3 -> 
								ValueAtScoreCardRowForA = lists:nth(ScorecardAChoice, PlayerAScoreCard),
								ValueAtScoreCardRowForB = lists:nth(ScorecardBChoice, PlayerBScoreCard),
								%check if the slots are already taken

								if 
									ValueAtScoreCardRowForA =/= -1
											-> io:format("A cheated~n");
									true 	-> io:format("A has a valid move~n")
								end,

								NewPlayerAScore = score_logic(ScorecardAChoice, lists:nth(ScorecardAChoice, PlayerAScoreCard), DieToA),

								%Mark A's score card
								NewScorecardA = element(1, lists:split(ScorecardAChoice-1, PlayerAScoreCard)) ++ 
												[NewPlayerAScore] ++ 
												element(2, lists:split(ScorecardAChoice, PlayerAScoreCard)),

								io:format("Player A's scorecard is: ~p~n", [NewScorecardA]),
								io:format("Player A's choice is: ~p~n", [ScorecardAChoice]),
								io:format("Player A's die is: ~p~n", [DieToA]),
								io:format("Player A scores: ~p~n", [NewPlayerAScore]),


								%check if the slots are already taken
								if ValueAtScoreCardRowForB =/= -1 
											-> io:format("B cheated");
									true 	-> io:format("B has a valid move")
								end,

								NewPlayerBScore = score_logic(ScorecardBChoice, lists:nth(ScorecardBChoice, PlayerBScoreCard), DieToB),

								NewScorecardB = element(1, lists:split(ScorecardBChoice-1, PlayerBScoreCard)) ++ 
												[NewPlayerBScore] ++ 
												element(2, lists:split(ScorecardBChoice, PlayerBScoreCard)),

								io:format("Player B's scorecard is: ~p~n", [NewScorecardB]),
								io:format("Player B's choice is: ~p~n", [ScorecardBChoice]),
								io:format("Player B's die is: ~p~n", [DieToB]),
								io:format("Player B scores: ~p~n", [NewPlayerBScore]),

								TotalScoreForA = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerAScoreCard),
								TotalScoreForB = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerBScoreCard),

								handle_roll(	Tid, 
												Gid, 
												Roll+1, 
												PlayerAName, PlayerBName, 
												NewScorecardA, NewScorecardB, 
												DieOutcomesA, DieOutcomesB, 
												PidA, PidB, 
												DiceToKeepA, DiceToKeepB);
							true -> 
								handle_roll(	Tid, 
												Gid, 
												Roll+1, 
												PlayerAName, PlayerBName, 
												PlayerAScoreCard, PlayerBScoreCard, 
												DieOutcomesA, DieOutcomesB, 
												PidA, PidB, 
												DiceToKeepA, DiceToKeepB)
							end;

						_ -> io:format("Invalid message type")
					end;
				_ -> io:format("Invalid message type")
			end
		end.

checkIfYahtzeeBonusApplicable(Die, Scorecard) ->
	YahtzeeScore = lists:nth(?YAHTZEEINDEX, Scorecard),
	if YahtzeeScore =/= -1 -> 
		FirstVal = lists:nth(1, Die),
		ExponentOfFirstVal = math:pow(FirstVal, ?NEXTDIE),
		MultiplicationOfElements = lists:foldl(fun(X, Accin) -> X*Accin end, 1, Die),

		if 	ExponentOfFirstVal == MultiplicationOfElements -> 
				%Yahtzee!
				PreviousBonusVal = lists:nth(?BONUSINDEX, Scorecard),
				NewBonusVal = PreviousBonusVal + 100,
				NewScoreCard = element(1, lists:split(?BONUSINDEX-1, Scorecard)) ++ 
							[NewBonusVal] ++ element(2, lists:split(?BONUSINDEX, Scorecard));
			true -> Scorecard
		end;
	true -> Scorecard
end.



%this method is used to find the next roll to die to pass on to the players
send_die_from_choice(DieSequence, Choice, Roll, CurrentIndex, AccumulatedDieSeq) ->
	% io:format("The die sequence is ~p~n", [DieSequence]),
	% io:format("The current index is ~p~n", [CurrentIndex]),
	% io:format("The choice is ~p~n", [Choice]),
	if  CurrentIndex > length(Choice) -> AccumulatedDieSeq;
	true -> 
		Boolean = lists:nth(CurrentIndex, Choice),
		if Boolean == false -> 
			% io:format("The die sequence inside the if statement is ~p~n", [DieSequence]),
			NextIndex = CurrentIndex*(Roll-1) + ?NEXTDIE,
			% io:format("The next index is: ~p~n", [NextIndex]),
			Problem = lists:nth(NextIndex, DieSequence),
			% io:format("Problem is: ~p~n", [Problem]),
			NewAccumulatedDieSeq = AccumulatedDieSeq ++ [lists:nth(NextIndex, DieSequence)],
			send_die_from_choice(DieSequence, Choice, Roll, CurrentIndex+1, NewAccumulatedDieSeq);
		true -> 
			NewAccumulatedDieSeq = AccumulatedDieSeq ++ [lists:nth(CurrentIndex, DieSequence)],
			send_die_from_choice(DieSequence, Choice, Roll, CurrentIndex+1, NewAccumulatedDieSeq)
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

getName() ->
  ?GLOBALNAME ++ io_lib:format("~p", [self()]).

printnameln(ToPrint) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint).

printnameln(ToPrint, Options) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint, Options).
