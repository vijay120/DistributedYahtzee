%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(yahtzee_player1).
-import(yahtzee_manager, [println/1, println/2]).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([main/1, bestMove/2, generateDiceChanges/2, generateDiceRolls/3, fixFlatten/2, 
		 getExpectedScore/3, playMove/8, calcUpper/3, calcThreeKind/2, calcFourKind/2,
		 calcFullHouse/2, calcSmallStraight/2, calcLargeStraight/2, calcYahtzee/2,
		 calcChance/2]).
%% ====================================================================
%%                             Constants
%% ====================================================================
-define(GLOBALNAME, "YahtzeePlayer1").
-define(TIMEOUT, 2000).

-define(ACES, 1).
-define(TWOS, 2).
-define(THREES, 3).
-define(FOURS, 4).
-define(FIVES, 5).
-define(SIXES, 6).
-define(THREEKIND, 7).
-define(FOURKIND, 8).
-define(FULLHOUSE, 9).
-define(SMALLSTRAIGHT, 10).
-define(LARGESTRAIGHT, 11).
-define(YAHTZEE, 12).
-define(CHANCE, 13).

%% ====================================================================
%%                            Main Function
%% ====================================================================
main(Params) ->
	printnameln("In main"),
	%set up network connections
	os:cmd("epmd -daemon"),
	RegName =     hd(Params),
	Username =    hd(tl(Params)),
	Password =    hd(tl(tl(Params))),
	SysManagers = tl(tl(tl(Params))), % will be a list of them

	SystManagersAtoms = lists:map(fun(X) -> list_to_atom(X) end, SysManagers),
	printnameln("before netstart"),
	io:format("My pid is ~p", [self()]),
	net_kernel:start([list_to_atom(RegName), shortnames]),
	register(player, self()), % Useful for testing to send message by node name

	% register with all system managers
	printnameln("Just before send SysManagers: ~p", [SystManagersAtoms]),
	lists:map(fun(X) -> net_kernel:connect_node(X) end, SystManagersAtoms),
	lists:map(fun(X) -> {yahtzee_manager, X} !
		{login, self(), node(), {Username, Password}} end, SystManagersAtoms),

	printnameln("My PID is: ~p", [self()]),

	handleMessages(Username, [], [], false).


% Username:		As specified on the command line.
% ActiveTids: 	A list of Tids.
% LoginTickets: A list of tuples, {Pid, LoginTicket}, so we can associate it with the
% 				proper system manager. (As per Moodle protocol clarification.)
% IsLoggingOut: A boolean if we want to log out, in which case we reject
% 				any new tournaments and wait until all our active 
%				tourneys are done, and then finally log out.
handleMessages(Username, LoginTickets, ActiveTids, IsLoggingOut) ->
	printnameln("In handleMessages with username: ~p, LoginTickets: ~p, ActiveTids: ~p",
			  						  [Username, LoginTickets, ActiveTids]),
		% Logs us out if we want to and are in no active tournaments.
	if  % Currently this will never run as we never actually want to
	    % programmatically log out.
		IsLoggingOut and (ActiveTids == []) ->
			lists:map(
				fun({SystemPid, LoginTicket}) ->
					printnameln("Logging out from SystemPid ~p", [SystemPid]),
					SystemPid ! {logout, self(), Username, {LoginTicket}},
					printnameln("Logged out from SystemPid ~p with LoginTicket ~p!",
						[SystemPid, LoginTicket])
				end,
				LoginTickets
			),
			halt(0);
		true ->
			true
	end,

	receive
		{please_logout, Pid, Username, {}} ->
			printnameln("Received a please_logout message"),
			Pid ! {logged_out, self(), Username, {}},
			handleMessages(Username, LoginTickets, ActiveTids, true);

		{logged_in, Pid, Username, {NewLoginTicket}} ->
			printnameln("Received a logged_in message"),
			handleMessages(Username, [{Pid, NewLoginTicket} | LoginTickets], ActiveTids, IsLoggingOut);

		{start_tournament, Yid, Username, {Tid}} ->
			printnameln("Received a start_tournament message with Yid = ~p, Tid = ~p", [Yid, Tid]),
			Result = lists:keyfind(Yid, 1, LoginTickets),
			NewActiveTids = case Result of
				false ->
					printnameln("No PID matches this login ticket."),
					ActiveTids;
				{_Key, ProperLoginTicket} ->
					printnameln("PID matches the login ticket!"),
					case IsLoggingOut of
						true ->
							printnameln("Is logging out! Sending reject_tournament..."),
							Yid ! {reject_tournament, self(), Username, {Tid, ProperLoginTicket}},
							ActiveTids;
						false ->
							printnameln("Is not logging out! Sending accept_tournament..."),
							Yid ! {accept_tournament, self(), Username, {Tid, ProperLoginTicket}},
							[Tid|ActiveTids]
					end;
				_ ->
					printnameln("Bad code.")
			end,
			handleMessages(Username, LoginTickets, NewActiveTids, IsLoggingOut);

		{end_tournament, _, Username, {Tid}} ->
			printnameln("Received an end_tournament message"),
			NewActiveTids = lists:delete(Tid, ActiveTids),
			handleMessages(Username, LoginTickets, NewActiveTids, IsLoggingOut);

		{play_request, Pid, Username, 
			{Ref, Tid, Gid, RollNumber, Dice, Scorecard, _}} ->
			printnameln("Pid for sender should be: ~p", [Pid]),
			printnameln("Received a play_request message"),
			playMove(Pid, Username, Ref, Tid, Gid, RollNumber, Dice, Scorecard),
			handleMessages(Username, LoginTickets, ActiveTids, IsLoggingOut);
		Message ->
			printnameln("Received malformed message: ~p", [Message]),
			handleMessages(Username, LoginTickets, ActiveTids, IsLoggingOut)
	end.

% Handles all the logic for determining what dice to keep
% and what move to make, by calculating the expected value of all
% possible arrangements and choosing the best of those,
% then sending this reply back to the Pid that sent play_request.
playMove(Pid, Username, Ref, Tid, Gid, RollNumber, Dice, Scorecard) ->
	if
		RollNumber == 3 -> % if on last roll, just give the best move we can do.
			[_, Move] = bestMove(Dice, Scorecard),
			KeepAllDice = [true, true, true, true, true],
			Pid ! {play_action, self(), Username, {Ref, Tid, Gid, RollNumber, KeepAllDice, Move}};
		true -> % get expected value of keeping each permutation of die
			[KeepScore, KeepMove] = bestMove(Dice, Scorecard),
			KeepAllDice = [true, true, true, true, true],
			% printnameln("KeepScore: ~p, KeepMove: ~p", [KeepScore, KeepMove]),
			AllDiceChanges = fixFlatten(lists:flatten(generateDiceChanges(5, [])), []), % gets 2^5 lists of all dice keep/change
			% printnameln("AllDiceChanges: ~p", [AllDiceChanges]),
			% gets the expected score for all possible dice change configurations
			ExpectedScores = lists:map(fun(X) -> getExpectedScore(X, Dice, Scorecard) end, AllDiceChanges),
			% printnameln("ChangesScores are: ~p", [ExpectedScores]),
			MaxExpectedScore = lists:max(ExpectedScores),
			if
				MaxExpectedScore > KeepScore -> % then we use that change
					Index = indexOf(MaxExpectedScore, ExpectedScores),
					ChangesToMake = lists:nth(Index, AllDiceChanges),
					printnameln("MaxExpectedScore is: ~p", [MaxExpectedScore]),
					printnameln("KeepScore is: ~p", [KeepScore]),
					printnameln("Changes in die to make are: ~p", [ChangesToMake]), % KeepMove doesn't actually matter here
					Pid ! {play_action, self(), Username, {Ref, Tid, Gid, RollNumber, ChangesToMake, KeepMove}};
				true ->
					printnameln("Best move is: ~p", [KeepMove]), 
					Pid ! {play_action, self(), Username, {Ref, Tid, Gid, RollNumber, KeepAllDice, KeepMove}}
			end
	end.

% Given a particular set of changes, dice and scorecard, calculates the expected value of those changes,
% which is the average of all possible configurations that can result from this.
getExpectedScore(DiceChanges, Dice, Scorecard) ->
	PossibleDieRolls = fixFlatten(lists:flatten(generateDiceRolls(DiceChanges, Dice, [])), []), % generate all dice rolls
	% printnameln("PossibleDieRolls are: ~p", [PossibleDieRolls]),
	ListDieScores = lists:map(fun(X) -> Y = bestMove(X, Scorecard), lists:nth(1, Y) end, PossibleDieRolls),
	% printnameln("ListDieScores is: ~p", [ListDieScores]),
	lists:sum(ListDieScores) / length(ListDieScores).

% Gets a flattened list of dice or changes to make,
% outputs a list of lists, where each sublist is 5 die or changes.
fixFlatten(List, NewList) ->
	if
		List == [] ->
			NewList;
		true ->
			fixFlatten(lists:sublist(List, 6, length(List)), [lists:sublist(List, 5)| NewList])
	end.

% Generates all combinations of booleans, as a mangled list of lists.
% Can be flattened and then take five at a time to create a proper list of lists.
% Note that that is handled on the caller side.
generateDiceChanges(NumLeft, AccumList) ->
	if
		NumLeft == 0 ->
			AccumList;
		true ->
			[generateDiceChanges(NumLeft-1, [true | AccumList]), generateDiceChanges(NumLeft-1, [false | AccumList])]
	end.

% Given a set of changes to make and the current state of the dice,
% return a mangled list of lists of all possible dice rolls for those
% changes.
generateDiceRolls(DiceChanges, Dice, AccumList) ->
	if
		DiceChanges == [] ->
			lists:reverse(AccumList);
		true ->
			if
				hd(DiceChanges) == true -> % Just keep this die
					[generateDiceRolls(tl(DiceChanges), tl(Dice), [hd(Dice) | AccumList])];
				true -> % case of changing it, generate all other dice rolls then our current die.
					[generateDiceRolls(tl(DiceChanges), tl(Dice), [transformVal(hd(Dice) + 1) | AccumList]), 
					 generateDiceRolls(tl(DiceChanges), tl(Dice), [transformVal(hd(Dice) + 2) | AccumList]),
					 generateDiceRolls(tl(DiceChanges), tl(Dice), [transformVal(hd(Dice) + 3) | AccumList]), 
					 generateDiceRolls(tl(DiceChanges), tl(Dice), [transformVal(hd(Dice) + 4) | AccumList]),
					 generateDiceRolls(tl(DiceChanges), tl(Dice), [transformVal(hd(Dice) + 5) | AccumList])]
			end		
	end.

% Makes it so that we never end up with our original value. 
% We add 1,2,3,4, and 5 as above; if we end up past 6,
% we change it to that num - 6. Can't just do a mod
% because we never want 0.
transformVal(Num) ->
	if
		Num < 7 ->
			Num;
		true ->
			Num - 6
	end.

% Given a particular set of dice and the current scorecard, return the best score
% and the move on the scorecard that gives it.
bestMove(Dice, Scorecard) ->
	AcesScore = calcUpper(Dice, lists:nth(?ACES, Scorecard), 1),
	TwosScore = calcUpper(Dice, lists:nth(?TWOS, Scorecard), 2),
	ThreesScore = calcUpper(Dice, lists:nth(?THREES, Scorecard), 3),
	FoursScore = calcUpper(Dice, lists:nth(?FOURS, Scorecard), 4), 
	FivesScore = calcUpper(Dice, lists:nth(?FIVES, Scorecard), 5),
	SixesScore = calcUpper(Dice, lists:nth(?SIXES, Scorecard), 6),

	ThreeKindScore = calcThreeKind(Dice, lists:nth(?THREEKIND, Scorecard)),
	FourKindScore = calcFourKind(Dice, lists:nth(?FOURKIND, Scorecard)),
	FullHouseScore = calcFullHouse(Dice, lists:nth(?FULLHOUSE, Scorecard)),
	SmallStraightScore = calcSmallStraight(Dice, lists:nth(?SMALLSTRAIGHT, Scorecard)),
	LargeStraightScore = calcLargeStraight(Dice, lists:nth(?LARGESTRAIGHT, Scorecard)),
	YahtzeeScore = calcYahtzee(Dice, lists:nth(?YAHTZEE, Scorecard)),
	ChanceScore = calcChance(Dice, lists:nth(?CHANCE, Scorecard)),

	AllScores = [AcesScore, TwosScore, ThreesScore, FoursScore, FivesScore, SixesScore, 
			    ThreeKindScore, FourKindScore, FullHouseScore, SmallStraightScore, 
			    LargeStraightScore, YahtzeeScore, ChanceScore],
	
	MaxScore = lists:max(AllScores),
	MaxMove = indexOf(MaxScore, AllScores),

	% special case of a three of a kind when we should've done four of a kind
	% (because it is rarer, even though they give same point value)
	case ((MaxMove == ?THREEKIND) and (MaxScore == lists:nth(?FOURKIND, AllScores))) of
		true ->
			AdjustedMaxMove = ?FOURKIND;
		false ->
			AdjustedMaxMove = MaxMove
	end,

	% printnameln("MaxScore is: ~p, MaxMove is: ~p", [MaxScore, AdjustedMaxMove]),
	[MaxScore, AdjustedMaxMove].

% Given the dice and scorecard index value and number on the upper board (i.e. ones, twos...sixes)
% Calculate the score for that particular move.
calcUpper(Dice, Score, Num) ->
	if
		Score /= -1 ->
			NewScore = -1;
		true ->
			NewScore = lists:sum(lists:filter(fun(X) -> X == Num end, Dice))
	end,
	NewScore.

% Given dice and scorecard, calculate the score for a three of a kind.
% (A three of a kind can also be a valid four of a kind or Yahtzee.)
calcThreeKind(Dice, Score) ->
	if
		Score /= -1 ->
			-1;
		true -> % take first three die; filter original five rolls from these.
				% if the length of any of these are three or greater, we've
				% met the condition.
			FirstDieMatch = lists:filter(fun(X) -> X == lists:nth(1, Dice) end, Dice),
			SecondDieMatch = lists:filter(fun(X) -> X == lists:nth(2, Dice) end, Dice),
			ThirdDieMatch = lists:filter(fun(X) -> X == lists:nth(3, Dice) end, Dice),
			MaxLength = lists:max([length(FirstDieMatch), length(SecondDieMatch), length(ThirdDieMatch)]),

			if
				MaxLength >= 3 ->
					lists:sum(Dice);
				true ->
					0
			end
	end.

% Given dice and scorecard, calculate score for a four of a kind.
% (A four of a kind can also be a valid Yahtzee.)
calcFourKind(Dice, Score) ->
	if
		Score /= -1 ->
			-1;
		true -> % take first two die; filter original five rolls from these.
				% if the length of any of these are four or greater, we've
				% met the condition.
			FirstDieMatch = lists:filter(fun(X) -> X == lists:nth(1, Dice) end, Dice),
			SecondDieMatch = lists:filter(fun(X) -> X == lists:nth(2, Dice) end, Dice),
			MaxLength = lists:max([length(FirstDieMatch), length(SecondDieMatch)]),

			if
				MaxLength >= 4 ->
					lists:sum(Dice);
				true ->
					0
			end
	end.

% Given dice and scorecard, calculate score for a full house.
% A full house is a three of a kind and a two of a kind.
calcFullHouse(Dice, Score) ->
	if
		Score /= -1 ->
			-1;
		true -> % Filter first by one die, then a second. 
				% If the length of the first filter leaves 2 or 3 die
				% and the second gets the rest, we have a full house.
			FirstDieUnmatch = lists:filter(fun(X) -> X /= lists:nth(1, Dice) end, Dice),
			SecondDieUnMatch = lists:filter(fun(X) -> X /= lists:nth(1, FirstDieUnmatch) end, FirstDieUnmatch),
			LengthFirst = length(FirstDieUnmatch),
	
			case ((LengthFirst == 2) or (LengthFirst == 3)) and (SecondDieUnMatch == []) of
				true ->
					25;
				false ->
					0
			end
	end.

% Given dice and scorecard, calculate score for small straight.
% Small straight: x,x+1,x+2,x+3,y (y can be anything)
calcSmallStraight(Dice, Score) ->
	DiceSet = ordsets:from_list(Dice),
	if
		Score /= -1 ->
			-1;
		true -> 
			Set1 = ordsets:from_list([1,2,3,4]), % all possible sets for the
			Set2 = ordsets:from_list([2,3,4,5]), % small straight. See if one
			Set3 = ordsets:from_list([3,4,5,6]), % in the original die roll.
			case (ordsets:is_subset(Set1, DiceSet)) or (ordsets:is_subset(Set2, DiceSet)) or (ordsets:is_subset(Set3, DiceSet)) of
				true ->
					30;
				false ->
					0
			end
	end.

% Given dice and scorecard, calculate score for large straight.
% Large straight: x,x+1,x+2,x+3,x+4
calcLargeStraight(Dice, Score) ->
	DiceSet = ordsets:from_list(Dice),
	if
		Score /= -1 ->
			-1;
		true ->
			Set1 = ordsets:from_list([1,2,3,4,5]),
			Set2 = ordsets:from_list([2,3,4,5,6]),
			case (ordsets:is_subset(Set1, DiceSet)) or (ordsets:is_subset(Set2, DiceSet)) of
				true ->
					40;
				false ->
					0
			end
	end.

% Given dice and scorecard, calculate score for Yahtzee. 
% Yahtzee: five of a kind.
calcYahtzee(Dice, Score) ->
	if
		Score /= -1 ->
			-1;
		true -> % filter out those that match first die, see if none left
			FirstDieUnmatch = lists:filter(fun(X) -> X /= lists:nth(1, Dice) end, Dice),
	
			case FirstDieUnmatch == [] of
				true ->
					50;
				false ->
					0
			end
	end.

% Calculate the total value of all dice. 
calcChance(Dice, Score) ->
	if
		Score /= -1 ->
			-1;
		true ->
			lists:sum(Dice)
	end.

% Gets the index of Item in List, or not_found if it's not there.
% Helper for calculating scores above.
indexOf(Item, List) -> indexOf(Item, List, 1).

indexOf(_, [], _)  -> not_found;
indexOf(Item, [Item|_], Index) -> Index;
indexOf(Item, [_|Tl], Index) -> indexOf(Item, Tl, Index+1).

%% ====================================================================
%%                       Pretty Print Functions
%% ====================================================================
getName() ->
  ?GLOBALNAME ++ io_lib:format("~p", [self()]).

printnameln(ToPrint) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint).

printnameln(ToPrint, Options) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint, Options).