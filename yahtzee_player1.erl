-module(yahtzee_player1).
-export([main/1, bestMove/2, generateDiceChanges/2, generateDiceRolls/3, fixFlatten/2, getExpectedScore/3, playMove/9]).
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

main(Params) ->
	%set up network connections
	_ = os:cmd("epmd -daemon"),
	Reg_name = hd(Params),
	Username = hd(tl(Params)),
	Password = hd(tl(tl(Params))),
	SysManagers = tl(tl(tl(Params))), % will be a list of them

	% TODO: as per ongoing Moodle discussion, sysmanagers register as yahtzee_manager
	% 		so we don't specify it on the command line.

	net_kernel:start([list_to_atom(Reg_name), shortnames]),
	register(player, self()), % TODO: name might change

	% register with all system managers. According to assignment the names are their
	% globally registered ones.
	io:format("Just before global send SysManagers: ~p~n", [SysManagers]),
	lists:map(fun(X) -> net_kernel:connect_node(X) end, SysManagers),
	lists:map(fun(X) -> {yahtzee_manager, X} ! {login, self(), Username, {Username, Password}} end, SysManagers),

	io:format("My PID is: ~p", [self()]),

	handleMessages(Username, [], [], false).

% IsLoggingOut: A boolean if we want to log out, in which case we reject
% any new tournaments and wait until all our active tourneys are done,
% and then finally log out.
%
% LoginTickets is now a tuple, {Pid, LoginTicket}, so we can associate it with the 
% proper system manager. (As per Moodle protocol clarification.)
handleMessages(Username, LoginTickets, ActiveTids, IsLoggingOut) ->
	io:format("In handleMessages with username: ~p, LoginTicketss: ~p, ActiveTids: ~p~n",
			  						  [Username, LoginTickets, ActiveTids]),
		% Logs us out if we want to and are in no active tournaments.
	if  % Currently this will never run as we never actually want to
	    % programmatically log out.
		IsLoggingOut and ActiveTids == [] ->
			lists:map(fun({Pid, LoginTicket}) -> Pid ! {logout, self(), Username, {LoginTicket}} end, LoginTickets);
			% lists:map(fun(X) -> global:send(X, {logout, self(), Username, {LoginTickets}}) end, SysManagers);
		true ->
			true
	end,

	receive
		{logged_in, Pid, Username, {NewLoginTicket}} ->
			io:format("Received a logged_in message~n"),
			handleMessages(Username, [{Pid, NewLoginTicket} | LoginTickets], ActiveTids, IsLoggingOut);
		{start_tournament, Pid, Username, {Tid}} ->
			io:format("Received a start_tournament message~n"),
			{_, ProperLoginTicket} = lists:keyfind(Pid, 1, LoginTickets),
			if
				ProperLoginTicket == false ->
					io:format("No PID matches this login ticket."),
					NewActiveTids = ActiveTids;
				true ->
					if
						IsLoggingOut ->
							NewActiveTids = ActiveTids,
							Pid ! {reject_tournament, self(), Username, {Tid, ProperLoginTicket}};
						true ->
							NewActiveTids = [Tid|ActiveTids], 
							Pid ! {accept_tournament, self(), Username, {Tid, ProperLoginTicket}}
					end
			end,
			handleMessages(Username, LoginTickets, NewActiveTids, IsLoggingOut);
		{end_tournament, Pid, Username, {Tid}} ->
			io:format("Received an end_tournament message~n"),
			NewActiveTids = lists:delete(Tid, ActiveTids),
			handleMessages(Username, LoginTickets, NewActiveTids, IsLoggingOut);
		{play_request, Pid, Username, 
			{Ref, Tid, Gid, RollNumber, Dice, Scorecard, OppScorecard}} ->
			playMove(Pid, Username, Ref, Tid, Gid, RollNumber, Dice, Scorecard, OppScorecard),
			io:format("Received a play_request message~n");
		Message ->
			io:format("Received malformed message: ~p~n", [Message]),
			handleMessages(Username, LoginTickets, ActiveTids, IsLoggingOut)
	end.

% Handles all the logic for determining what dice to keep
% and what move to make, by calculating the expected value of all
% possible arrangements and choosing the best of those.
playMove(Pid, Username, Ref, Tid, Gid, RollNumber, Dice, Scorecard, OppScorecard) ->
	if
		RollNumber == 3 -> % if on last roll, just give the best move we can do.
			[_, Move] = bestMove(Dice, Scorecard),
			KeepAllDice = [true, true, true, true, true],
			Pid ! {play_action, self(), Username, {Ref, Tid, Gid, RollNumber, KeepAllDice, Move}};
		true -> % get expected value of keeping each permutation of die
			[KeepScore, KeepMove] = bestMove(Dice, Scorecard),
			KeepAllDice = [true, true, true, true, true],
			% io:format("KeepScore: ~p, KeepMove: ~p", [KeepScore, KeepMove]),
			AllDiceChanges = fixFlatten(lists:flatten(generateDiceChanges(5, [])), []), % gets 2^5 lists of all dice keep/change
			% io:format("AllDiceChanges: ~p", [AllDiceChanges]),
			% gets the expected score for all possible dice change configurations
			ExpectedScores = lists:map(fun(X) -> getExpectedScore(X, Dice, Scorecard) end, AllDiceChanges),
			% io:format("ChangesScores are: ~p", [ExpectedScores]),
			MaxExpectedScore = lists:max(ExpectedScores),
			if
				MaxExpectedScore > KeepScore -> % then we use that change
					Index = indexOf(MaxExpectedScore, ExpectedScores),
					ChangesToMake = lists:nth(Index, AllDiceChanges),
					io:format("MaxExpectedScore is: ~p", [MaxExpectedScore]),
					io:format("KeepScore is: ~p", [KeepScore]),
					io:format("Changes in die to make are: ~p", [ChangesToMake]), % KeepMove doesn't actually matter here
					Pid ! {play_action, self(), Username, {Ref, Tid, Gid, RollNumber, ChangesToMake, KeepMove}};
				true ->
					io:format("Best move is: ~p", [KeepMove]), 
					Pid ! {play_action, self(), Username, {Ref, Tid, Gid, RollNumber, KeepAllDice, KeepMove}}
			end
	end.

% Given a particular set of changes, dice and scorecard, calculates the expected value of those changes,
% which is the average of all possible configurations that can result from this.
getExpectedScore(DiceChanges, Dice, Scorecard) ->
	PossibleDieRolls = fixFlatten(lists:flatten(generateDiceRolls(DiceChanges, Dice, [])), []), % generate all dice changes
	% io:format("PossibleDieRolls are: ~p~n", [PossibleDieRolls]),
	ListDieScores = lists:map(fun(X) -> Y = bestMove(X, Scorecard), lists:nth(1, Y) end, PossibleDieRolls), % get all the best moves
	% io:format("got here"),
	% io:format("ListDieScores is: ~p~n", [ListDieScores]),
	% Sum = lists:sum(ListDieScores),
	% % io:format("Sum is: ~p~n", [Sum]),
	% Length = length(ListDieScores),
	% % io:format("Length is: ~p~n", [Length]),
	% Sum / length(ListDieScores).
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

% Generates all combinations of booleans, as a really mangled list of lists.
% Can be flattened and then take five at a time to create a proper list of lists.
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

	% io:format("MaxScore is: ~p, MaxMove is: ~p~n", [MaxScore, AdjustedMaxMove]),
	[MaxScore, AdjustedMaxMove].

% Given the dice and scorecard and number on the upper board (i.e. ones, twos...sixes)
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