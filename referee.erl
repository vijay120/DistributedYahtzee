%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(referee).

-import(yahtzee_manager, [println/1, println/2]).
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
-define(NOWINS, 0).


-define(PID, 1).
-define(NODE, 2).
-define(USERNAME, 3).
-define(PASSWORD, 4).
-define(LOGIN_TICKET, 5).
-define(IS_LOGIN, 6).
-define(MATCH_WINS, 7).
-define(MATCH_LOSSES, 8).
-define(TOURNAMENTS_PLAYED, 9).
-define(TOURNAMENTS_WIN, 10).

%% ====================================================================
%%                            Main Function
%% ====================================================================
referee_main(Params) ->
  os:cmd("epmd -daemon"),
  Reg_name = hd(Params),
  PlayerATuple  = lists:nth(1, hd(tl(Params))),
  PlayerBTuple = lists:nth(2, hd(tl(Params))),
  printnameln("Player A tuple is ~p", [PlayerATuple]),
  printnameln("Player B tuple is ~p", [PlayerBTuple]),
  TournamentId = hd(tl(tl(Params))),

  net_kernel:start([list_to_atom(Reg_name), shortnames]),

  printnameln("My node is ~p", [node()]),
  printnameln("My pid is ~p", [self()]),
  register(referee, self()),
  findMyPlayersAndGameId(PlayerATuple, PlayerBTuple, TournamentId).


findMyPlayersAndGameId(PlayerATuple, PlayerBTuple, TournamentId) ->
  GameId = self(),
  PlayerAPid = element(?PID, PlayerATuple),
  PlayerAName = element(?USERNAME, PlayerATuple),
  PlayerANode = element(?NODE, PlayerATuple),
  PlayerBPid = element(?PID, PlayerBTuple),
  PlayerBName = element(?USERNAME, PlayerBTuple),
  PlayerBNode = element(?NODE, PlayerBTuple),

  random:seed(now()),
  timer:sleep(100),
  ScorecardA = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
  ScorecardB = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),

  handle_game(?FIRSTROUND, 
        TournamentId, 
        GameId, 
        PlayerAName, PlayerBName, 
        ScorecardA, ScorecardB, 
        PlayerANode, PlayerBNode).

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

handle_game(
  Round,
  Tid, 
  Gid, 
  PlayerAName, PlayerBName, 
  PlayerAScoreCard, PlayerBScoreCard, 
  PlayerANode, PlayerBNode
) ->  
  printnameln("In handle game"),

  if Round > 13 -> 
      TotalScoreForA = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerAScoreCard),
      TotalScoreForB = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerBScoreCard),

      printnameln("Game stats: PlayerAScore = ~p, PlayerBScore = ~p",
        [TotalScoreForA, TotalScoreForB]),
      printnameln("Game is done!"),
      % TODO: Calculate Total Player A and B scores

      {UserRecords, Winner} = if 
        TotalScoreForA > TotalScoreForB -> 
          UserRecordA = {PlayerAName, 1, 0},
          UserRecordB = {PlayerBName, 0, 1},
          {[UserRecordA, UserRecordB], PlayerBName};
        TotalScoreForA == TotalScoreForB -> % TODO: Two players tied! Replay entire game
                                        % TODO: with tie counter incremented. If
                                        % TODO: it reaches a certain point, replay
                                        % TODO: the game under standard Yahtzee rules?
          UserRecordA = {PlayerAName, 0, 0}, % ties are discarded
          UserRecordB = {PlayerBName, 0, 0},
          {[UserRecordA, UserRecordB], tie}; % my proposed protocol for ties
        TotalScoreForA < TotalScoreForB ->
          UserRecordA = {PlayerAName, 0, 1},
          UserRecordB = {PlayerBName, 1, 0},
          {[UserRecordA, UserRecordB], PlayerAName}
      end,
      Tid ! {report_game_results, self(), {UserRecords, Winner}};
  true -> 
    random:seed(now()),
    timer:sleep(100),
    % TODO EOIN: Shouldn't these be the same list? (Except for when they match, in
    %            case we do as it is doing?)
    DieOutcomesA = generate_fixed_length_lists("random", ?NUMPOSSIBLEDIEOUTCOMES),
    DieOutcomesB = DieOutcomesA,
    ChoiceA = ?INITIALDIECHOICE,
    ChoiceB = ?INITIALDIECHOICE,

    [NewPlayerAScoreCard, NewPlayerBScoreCard] =
      handle_roll(
        Tid, 
        Gid, 
        ?FIRSTROLL, 
        PlayerAName, PlayerBName, 
        PlayerAScoreCard, PlayerBScoreCard, 
        DieOutcomesA, DieOutcomesB, 
        PlayerANode, PlayerBNode, 
        ChoiceA, ChoiceB
      ),
    % NewPlayerAWins = PlayerAWins + InPlayerAWin, % TODO: This should be score, not wins.
    % NewPlayerBWins = PlayerBWins + InPlayerBWin, % TODO: The only win is at end of game and match.
    % NewPlayerAScore = PlayerAScore + InPlayerAScore,
    % NewPlayerBScore = PlayerBScore + InPlayerBScore

    handle_game(
      Round + 1,
      Tid, 
      Gid, 
      PlayerAName, PlayerBName, 
      NewPlayerAScoreCard, NewPlayerBScoreCard,
      PlayerANode, PlayerBNode
    )
  end.

  
handle_roll(
  Tid, 
  Gid, 
  Roll, 
  PlayerAName, PlayerBName, 
  PlayerAScoreCard, PlayerBScoreCard, 
  DieOutcomesA, DieOutcomesB,
  PlayerANode, PlayerBNode, 
  ChoiceA, ChoiceB
) ->

  printnameln("in handle roll"),

  if Roll > 3 ->
    %The last roll is over, so pass the results to the tournament manager
    % printnameln("Player A's score is: ~p", [PlayerAScore]),
    % printnameln("Player B's score is: ~p", [PlayerBScore]),
    TotalScoreForA = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerAScoreCard),
    TotalScoreForB = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerBScoreCard),

    [PlayerAScoreCard, PlayerBScoreCard];
    % if
    %   TotalScoreForA > TotalScoreForB ->
    %    [PlayerAScoreCard, PlayerBScoreCard, 1, 0];
    %   true ->
    %     [PlayerAScoreCard, PlayerBScoreCard, 0, 1]
    % end;

    true ->
      %Step 1: Calculate the dies that need to be send for each player
      DieToA = send_die_from_choice(DieOutcomesA, ChoiceA, Roll, ?STARTINDEX, []),
      DieToB = send_die_from_choice(DieOutcomesB, ChoiceB, Roll, ?STARTINDEX, []),

      ReplacedScoreCardA = checkIfYahtzeeBonusApplicable(DieToA, PlayerAScoreCard),
      ReplacedScoreCardB = checkIfYahtzeeBonusApplicable(DieToB, PlayerBScoreCard),

      %Step 2: Send the message!

      % WE HAD TO USE THE NODE ID SINCE THE PID'S COULD BE THE SAME!
      printnameln("Before message sent"),
      printnameln("Player a username is: ~p", [PlayerAName]),
      printnameln("Player b username is: ~p", [PlayerBName]),
      
      {player, PlayerANode} !
        {play_request, self(), PlayerAName,
          {make_ref(), Tid, Gid, Roll, DieToA, ReplacedScoreCardA, ReplacedScoreCardB}},
      {player, PlayerBNode} !
        {play_request, self(), PlayerBName,
          {make_ref(), Tid, Gid, Roll, DieToB, ReplacedScoreCardB, ReplacedScoreCardA}},
      printnameln("After message sent"),

      %Recieve for player A only
      receive
        {play_action, _, PlayerAName, {_, _, _, _, DiceToKeepA, ScorecardAChoice}} -> 
          %Receive for player B only
          receive
            {play_action, _, PlayerBName, {_, _, _, _, DiceToKeepB, ScorecardBChoice}} -> 

              %So do not care about any of the score update logic until roll 3
              if Roll == 3 -> 
                ValueAtScoreCardRowForA = lists:nth(ScorecardAChoice, PlayerAScoreCard),
                ValueAtScoreCardRowForB = lists:nth(ScorecardBChoice, PlayerBScoreCard),
                %check if the slots are already taken

                if 
                  ValueAtScoreCardRowForA =/= -1 ->
                    printnameln("A cheated");
                  true  ->
                    printnameln("A has a valid move")
                end,

                NewPlayerAScore = score_logic(ScorecardAChoice,
                  lists:nth(ScorecardAChoice, PlayerAScoreCard), DieToA),

                %Mark A's score card
                NewScorecardA = element(1, lists:split(ScorecardAChoice - 1, PlayerAScoreCard)) ++ 
                  [NewPlayerAScore] ++ 
                  element(2, lists:split(ScorecardAChoice, PlayerAScoreCard)),

                printnameln("Player A's scorecard is: ~p", [NewScorecardA]),
                printnameln("Player A's choice is: ~p", [ScorecardAChoice]),
                printnameln("Player A's die is: ~p", [DieToA]),
                printnameln("Player A scores: ~p", [NewPlayerAScore]),


                %check if the slots are already taken
                if ValueAtScoreCardRowForB =/= -1 
                      -> printnameln("B cheated");
                  true  -> printnameln("B has a valid move")
                end,

                NewPlayerBScore = score_logic(ScorecardBChoice,
                  lists:nth(ScorecardBChoice, PlayerBScoreCard), DieToB),

                NewScorecardB = element(1, lists:split(ScorecardBChoice-1, PlayerBScoreCard)) ++ 
                        [NewPlayerBScore] ++ 
                        element(2, lists:split(ScorecardBChoice, PlayerBScoreCard)),

                printnameln("Player B's scorecard is: ~p", [NewScorecardB]),
                printnameln("Player B's choice is: ~p", [ScorecardBChoice]),
                printnameln("Player B's die is: ~p", [DieToB]),
                printnameln("Player B scores: ~p", [NewPlayerBScore]),

                TotalScoreForA = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerAScoreCard),
                TotalScoreForB = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerBScoreCard),

                handle_roll(
                  Tid, 
                  Gid, 
                  Roll+1, 
                  PlayerAName, PlayerBName, 
                  NewScorecardA, NewScorecardB, 
                  DieOutcomesA, DieOutcomesB, 
                  PlayerANode, PlayerBNode, 
                  DiceToKeepA, DiceToKeepB
                );
              true -> 
                handle_roll(
                  Tid, 
                  Gid, 
                  Roll+1, 
                  PlayerAName, PlayerBName, 
                  PlayerAScoreCard, PlayerBScoreCard, 
                  DieOutcomesA, DieOutcomesB, 
                  PlayerANode, PlayerBNode, 
                  DiceToKeepA, DiceToKeepB
                )
              end;
            InvalidMessage -> printnameln("Invalid message: ~p", [InvalidMessage])
          end;
          {play_action, _, PlayerBName, {_, _, _, _, DiceToKeepB, ScorecardBChoice}} -> 
            %Receive for player B only
            receive
              {play_action, _, PlayerAName, {_, _, _, _, DiceToKeepA, ScorecardAChoice}} -> 

                %So do not care about any of the score update logic until roll 3
                if Roll == 3 -> 
                  ValueAtScoreCardRowForA = lists:nth(ScorecardAChoice, PlayerAScoreCard),
                  ValueAtScoreCardRowForB = lists:nth(ScorecardBChoice, PlayerBScoreCard),
                  %check if the slots are already taken

                  if 
                    ValueAtScoreCardRowForA =/= -1
                        -> printnameln("A cheated");
                    true  -> printnameln("A has a valid move")
                  end,

                  NewPlayerAScore = score_logic(ScorecardAChoice,
                    lists:nth(ScorecardAChoice, PlayerAScoreCard), DieToA),

                  %Mark A's score card
                  NewScorecardA = element(1, lists:split(ScorecardAChoice-1, PlayerAScoreCard)) ++ 
                          [NewPlayerAScore] ++ 
                          element(2, lists:split(ScorecardAChoice, PlayerAScoreCard)),

                  printnameln("Player A's scorecard is: ~p", [NewScorecardA]),
                  printnameln("Player A's choice is: ~p", [ScorecardAChoice]),
                  printnameln("Player A's die is: ~p", [DieToA]),
                  printnameln("Player A scores: ~p", [NewPlayerAScore]),


                  %check if the slots are already taken
                  if ValueAtScoreCardRowForB =/= -1 
                        -> printnameln("B cheated");
                    true  -> printnameln("B has a valid move")
                  end,

                  NewPlayerBScore = score_logic(ScorecardBChoice,
                    lists:nth(ScorecardBChoice, PlayerBScoreCard), DieToB),

                  NewScorecardB = element(1, lists:split(ScorecardBChoice-1, PlayerBScoreCard)) ++ 
                          [NewPlayerBScore] ++ 
                          element(2, lists:split(ScorecardBChoice, PlayerBScoreCard)),

                  printnameln("Player B's scorecard is: ~p", [NewScorecardB]),
                  printnameln("Player B's choice is: ~p", [ScorecardBChoice]),
                  printnameln("Player B's die is: ~p", [DieToB]),
                  printnameln("Player B scores: ~p", [NewPlayerBScore]),

                  TotalScoreForA = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerAScoreCard),
                  TotalScoreForB = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerBScoreCard),

                  handle_roll(  Tid, 
                          Gid, 
                          Roll+1, 
                          PlayerAName, PlayerBName, 
                          NewScorecardA, NewScorecardB, 
                          DieOutcomesA, DieOutcomesB, 
                          PlayerANode, PlayerBNode, 
                          DiceToKeepA, DiceToKeepB);
                true -> 
                  handle_roll(  Tid, 
                          Gid, 
                          Roll+1, 
                          PlayerAName, PlayerBName, 
                          PlayerAScoreCard, PlayerBScoreCard, 
                          DieOutcomesA, DieOutcomesB, 
                          PlayerANode, PlayerBNode, 
                          DiceToKeepA, DiceToKeepB)
                end;
              InvalidMessage -> printnameln("Invalid message: ~p", [InvalidMessage])
            end;
        InvalidMessage -> printnameln("Invalid message: ~p", [InvalidMessage])
      end
    end.


checkIfYahtzeeBonusApplicable(Die, Scorecard) ->
  YahtzeeScore = lists:nth(?YAHTZEEINDEX, Scorecard),
  if YahtzeeScore =/= -1 -> 
    FirstVal = lists:nth(1, Die),
    ExponentOfFirstVal = math:pow(FirstVal, ?NEXTDIE),
    MultiplicationOfElements = lists:foldl(fun(X, Accin) -> X*Accin end, 1, Die),

    if  ExponentOfFirstVal == MultiplicationOfElements -> 
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
  % printnameln("The die sequence is ~p", [DieSequence]),
  % printnameln("The current index is ~p", [CurrentIndex]),
  % printnameln("The choice is ~p", [Choice]),
  if  CurrentIndex > length(Choice) -> AccumulatedDieSeq;
  true -> 
    Boolean = lists:nth(CurrentIndex, Choice),
    if Boolean == false -> 
      % printnameln("The die sequence inside the if statement is ~p", [DieSequence]),
      NextIndex = CurrentIndex*(Roll-1) + ?NEXTDIE,
      % printnameln("The next index is: ~p", [NextIndex]),
      Problem = lists:nth(NextIndex, DieSequence),
      % printnameln("Problem is: ~p", [Problem]),
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
      true ->
        [element(1, random:uniform_s(?DIESTATE, random:seed(now())))] ++
        generate_fixed_length_lists(Type, Count-1)
    end
  end.

%% ====================================================================
%%                       Pretty Print Functions
%% ====================================================================
getName() ->
  ?GLOBALNAME ++ io_lib:format("~p", [self()]).

printnameln(ToPrint) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint).

printnameln(ToPrint, Options) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint, Options).
