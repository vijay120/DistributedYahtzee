-module(yahtzee_player1).
-export([main/1, bestMove/2]).
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

% Two main things: set up connection and register with tournament managers,
% handle game playing.

main(Params) ->
	%set up network connections
	_ = os:cmd("epmd -daemon"),
	Reg_name = hd(Params),
	Username = hd(tl(Params)),
	Password = hd(tl(tl(Params))),
	SysManagers = tl(tl(tl(Params))), % will be a list of them

	net_kernel:start([list_to_atom(Reg_name), shortnames]),
	register(player, self()), % TODO: name might change

	% register with all system managers. According to assignment the names are their
	% globally registered ones.
	io:format("Just before global send, SysManagers are: ~p~n", [SysManagers]),
	lists:map(fun(X) -> net_kernel:connect_node(list_to_atom(X)) end, SysManagers),
	global:sync(),
	lists:map(fun(X) -> global:send(X, {login, self(), Username, {Username, Password}}) end, SysManagers),

	handleMessages(Username, [], [], false, SysManagers).

% IsLoggingOut: A boolean if we want to log out, in which case we reject
% any new tournaments and wait until all our active tourneys are done,
% and then finally log out.
handleMessages(Username, LoginTicket, ActiveTids, IsLoggingOut, SysManagers) ->
	io:format("In handleMessages with username: ~p, LoginTickets: ~p, ActiveTids: ~p, SysManagers: ~p~n",
			  						  [Username, LoginTicket, ActiveTids, SysManagers]),
		% Logs us out if we want to and are in no active tournaments.
	if  % Currently this will never run as we never actually want to
	    % programmatically log out.
		IsLoggingOut and ActiveTids == [] ->
			lists:map(fun(X) -> global:send(X, {logout, self(), Username, {LoginTicket}}) end, SysManagers);
		true ->
			true
	end,

	receive
		{logged_in, Pid, Username, NewLoginTicket} ->
			io:format("Received a logged_in message~n"),
			handleMessages(Username, NewLoginTicket, ActiveTids, IsLoggingOut, SysManagers);
		{start_tournament, Pid, Username, Tid} ->
			io:format("Received a start_tournament message~n"),
			if
				IsLoggingOut ->
					{_, Tid} = lists:keysearch(Pid, 1, ActiveTids),
					NewActiveTids = ActiveTids,
					Pid ! {reject_tournament, self(), Username, {Tid, LoginTicket}};
				true ->
					NewActiveTids = [Tid|ActiveTids], 
					Pid ! {accept_tournament, self(), Username, {Tid, LoginTicket}}
			end,
			handleMessages(Username, LoginTicket, NewActiveTids, IsLoggingOut, SysManagers);
		{end_tournament, Pid, Username, Tid} ->
			io:format("Received an end_tournament message~n"),
			NewActiveTids = lists:delete(Tid, ActiveTids),
			handleMessages(Username, LoginTicket, NewActiveTids, IsLoggingOut, SysManagers);
		{play_request, Pid, Username, 
			{Ref, Tid, Gid, RollNumber, Dice, Scorecard, OppScorecard}} ->
			playMove(Pid, Username, Ref, Tid, Gid, RollNumber, Dice, Scorecard, OppScorecard),
			io:format("Received a play_request message~n");
		Message ->
			io:format("Received malformed message: ~p~n", [Message]),
			handleMessages(Username, LoginTicket, ActiveTids, IsLoggingOut, SysManagers)
	end.

% handles all the logic for calculating the best move (according to expected
% value), and sends that message back to the ref.
playMove(Pid, Username, Ref, Tid, Gid, RollNumber, Dice, Scorecard, OppScorecard) ->
	% make function that takes score card and dice, returns max value for that.
	% get this for our current score.
	if
		RollNumber == 3 -> % if on last roll, just give the best move we can do.
			[_, Move] = bestMove(Dice, Scorecard),
			KeepAllDice = [true, true, true, true, true],
			Pid ! {play_action, self(), Username, {Ref, Tid, Gid, RollNumber, KeepAllDice, Move}};
		true -> % get expected value of keeping each permutation of die
			[KeepScore, KeepMove] = bestMove(Dice, Scorecard),
			KeepAllDice = [true, true, true, true, true],
			Pid ! {play_action, self(), Username, {Ref, Tid, Gid, RollNumber, KeepAllDice, KeepMove}}
			% [OneChangeScore, OneChangeMove, DiceToChange] = oneChangeMove(Dice, Scorecard),
	end,

	% then get the maximum expected value from changing any one, two, three, four or
	% five die. If the max of these is higher than our current score, request
	% those rolls back. 
	% unless the rollNumber is the third, in which case just give back our best move.
	true.

% generate a separate list of each possible combination for each possible dice combo
% to keep, then average each to get all expected values. See if the max
% is greater than or less than our current best move?

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

	io:format("MaxScore is: ~p, MaxMove is: ~p~n", [MaxScore, AdjustedMaxMove]),
	[MaxScore, AdjustedMaxMove].

calcUpper(Dice, Score, Num) ->
	if
		Score /= -1 ->
			NewScore = -1;
		true ->
			NewScore = lists:sum(lists:filter(fun(X) -> X == Num end, Dice))
	end,
	NewScore.

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

calcFullHouse(Dice, Score) ->
	if
		Score /= -1 ->
			-1;
		true -> % take first two die; filter original five rolls from these.
				% if the length of any of these are four or greater, we've
				% met the condition.
			FirstDieUnmatch = lists:filter(fun(X) -> X /= lists:nth(1, Dice) end, Dice),
			SecondDieUnMatch = lists:filter(fun(X) -> X /= lists:nth(1, FirstDieUnmatch) end, FirstDieUnmatch),
			LengthFirst = length(FirstDieUnmatch),
	
			case ((LengthFirst == 2) or (LengthFirst == 3)) and (SecondDieUnMatch == []) of  % only have aaabb if first removed 2 or 3 that matched it and second got rest
				true ->
					25;
				false ->
					0
			end
	end.

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

calcYahtzee(Dice, Score) ->
	if
		Score /= -1 ->
			-1;
		true -> % filter out those that match first die, see if none left
			FirstDieUnmatch = lists:filter(fun(X) -> X /= lists:nth(1, Dice) end, Dice),
	
			case FirstDieUnmatch == [] of  % only have aaabb if first removed 2 or 3 that matched it and second got rest
				true ->
					50;
				false ->
					0
			end
	end.

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