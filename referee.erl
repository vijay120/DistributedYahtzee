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
-define(TIMEOUT, 60000).
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
  GamesPerMatch = hd(tl(tl(tl(Params)))),
  printnameln("READMEEOIN"),
  printnameln("GamesPerMatch is: ~p", [GamesPerMatch]),

  net_kernel:start([list_to_atom(Reg_name), shortnames]),

  printnameln("My node is ~p", [node()]),
  printnameln("My pid is ~p", [self()]),
  % register(referee, self()),
  findMyPlayersAndGameId(PlayerATuple, PlayerBTuple, TournamentId, GamesPerMatch).


findMyPlayersAndGameId(PlayerATuple, PlayerBTuple, TournamentId, GamesPerMatch) ->
  GameId = self(),
  _PlayerAPid = element(?PID, PlayerATuple),
  PlayerAName = element(?USERNAME, PlayerATuple),
  PlayerANode = element(?NODE, PlayerATuple),
  _PlayerBPid = element(?PID, PlayerBTuple),
  PlayerBName = element(?USERNAME, PlayerBTuple),
  PlayerBNode = element(?NODE, PlayerBTuple),

  random:seed(now()),
  timer:sleep(100),
  ScorecardA = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
  ScorecardB = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),

  handle_match(TournamentId, GameId, PlayerAName, PlayerBName, ScorecardA,
               ScorecardB, PlayerANode, PlayerBNode, GamesPerMatch, 0, 0, 0, false).

% Handles a match, reverting to standard yahtzee rules if needed.
% ConsecutiveTies: The number of games the two players have tied in a row.
% PlayerAWins and PlayerBWins are integers--the number of games won so far in the match.
handle_match(TournamentId, GameId, PlayerAName, PlayerBName, ScorecardA,
             ScorecardB, PlayerANode, PlayerBNode, GamesPerMatch, 
             ConsecutiveTies, PlayerAWins, PlayerBWins, IsStandard) ->
  if
    PlayerAWins > (GamesPerMatch div 2) ->
      UserRecordA = {PlayerAName, 1, 0},
      UserRecordB = {PlayerBName, 0, 1},
      UserRecords = [UserRecordA, UserRecordB],
      TournamentId ! {report_match_results, self(), {TournamentId, UserRecords, PlayerAName}};
    PlayerBWins > (GamesPerMatch div 2) ->
      UserRecordA = {PlayerAName, 0, 1},
      UserRecordB = {PlayerBName, 1, 0},
      UserRecords = [UserRecordA, UserRecordB],
      TournamentId ! {report_match_results, self(), {TournamentId, UserRecords, PlayerBName}};
    ConsecutiveTies > ((GamesPerMatch div 2) + 1) -> % Reset match under standard rules
      printnameln("****************************************"),
      printnameln("Now playing under standard Yahtzee rules"),
      printnameln("****************************************"),
      handle_match(TournamentId, GameId, PlayerAName, PlayerBName, ScorecardA, 
                       ScorecardB, PlayerANode, PlayerBNode, GamesPerMatch, 
                       0, 0, 0, true);

    true ->
      Winner = handle_game(?FIRSTROUND, 
                            TournamentId, 
                            GameId, 
                            PlayerAName, PlayerBName, 
                            ScorecardA, ScorecardB, 
                            PlayerANode, PlayerBNode,
                            IsStandard, false, false),

      if
        Winner == PlayerAName ->
          handle_match(TournamentId, GameId, PlayerAName, PlayerBName, ScorecardA,
                       ScorecardB, PlayerANode, PlayerBNode, GamesPerMatch, 
                       0, PlayerAWins+1, PlayerBWins, IsStandard);
        Winner == PlayerBName ->
          handle_match(TournamentId, GameId, PlayerAName, PlayerBName, ScorecardA,
                       ScorecardB, PlayerANode, PlayerBNode, GamesPerMatch, 
                       0, PlayerAWins, PlayerBWins+1, IsStandard);
        Winner == tie ->
          handle_match(TournamentId, GameId, PlayerAName, PlayerBName, ScorecardA,
                       ScorecardB, PlayerANode, PlayerBNode, GamesPerMatch, 
                       ConsecutiveTies+1, PlayerAWins, PlayerBWins, IsStandard);
        Winner == bye ->
          TournamentId ! {report_match_results, self(), {TournamentId, [{PlayerAName, 0, 0}, {PlayerBName, 0, 0}], bye}};
        true ->
          printnameln("Winner is neither player nor tie, it is: ~p", [Winner])
      end
  end.

%This function handles all the logic and enforcement of rules
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
  PlayerANode, PlayerBNode,
  IsStandard, IsBye, IsTimeOut
) ->  
  printnameln("Is timeout is ~p", [IsTimeOut]),

  if 
    PlayerAName == bye -> % If first is bye, other wins.
      PlayerBName;

    PlayerBName == bye -> % If second a bye, other wins.
      PlayerAName;

    IsBye == true ->
      printnameln("I am a bye!"),
      bye;

    (IsTimeOut == true) or (Round > 13)  -> 
      TotalScoreForA = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerAScoreCard),
      TotalScoreForB = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerBScoreCard),

      printnameln("Game stats: PlayerAScore = ~p, PlayerBScore = ~p",
        [TotalScoreForA, TotalScoreForB]),
      printnameln("Game is done!"),

      if 
        TotalScoreForA > TotalScoreForB -> 
          printnameln("~p wins!", [PlayerAName]),
          PlayerAName;
          % UserRecordA = {PlayerAName, 1, 0},
          % UserRecordB = {PlayerBName, 0, 1},
          % {[UserRecordA, UserRecordB], PlayerAName};
        TotalScoreForA == TotalScoreForB -> 
          printnameln("Both players tie"),
          tie;
          % UserRecordA = {PlayerAName, 0, 0}, % ties are discarded
          % UserRecordB = {PlayerBName, 0, 0},
          % {[UserRecordA, UserRecordB], tie}; % my proposed protocol for ties
        TotalScoreForA < TotalScoreForB ->
          printnameln("~p wins", [PlayerBName]),
          PlayerBName
          % UserRecordA = {PlayerAName, 0, 1},
          % UserRecordB = {PlayerBName, 1, 0},
          % {[UserRecordA, UserRecordB], PlayerBName}
      end;
      % Tid ! {report_match_results, self(), {UserRecords, Winner}};
  true -> 
    random:seed(now()),
    timer:sleep(100),
    if
      IsStandard ->
        DieOutcomesA = generate_fixed_length_lists("random", ?NUMPOSSIBLEDIEOUTCOMES),
        DieOutcomesB = generate_fixed_length_lists("random", ?NUMPOSSIBLEDIEOUTCOMES);
      true ->
        DieOutcomesA = generate_fixed_length_lists("random", ?NUMPOSSIBLEDIEOUTCOMES),
        DieOutcomesB = DieOutcomesA
    end,
    ChoiceA = ?INITIALDIECHOICE,
    ChoiceB = ?INITIALDIECHOICE,

    [NewPlayerAScoreCard, NewPlayerBScoreCard, NewIsBye, NewIsTimeOut] =
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

      

    handle_game(
      Round + 1,
      Tid, 
      Gid, 
      PlayerAName, PlayerBName, 
      NewPlayerAScoreCard, NewPlayerBScoreCard,
      PlayerANode, PlayerBNode,
      IsStandard, NewIsBye, NewIsTimeOut
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

  if Roll > 3 ->
    %The last roll is over, so pass the results to the tournament manager
    % printnameln("Player A's score is: ~p", [PlayerAScore]),
    % printnameln("Player B's score is: ~p", [PlayerBScore]),
    _TotalScoreForA = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerAScoreCard),
    _TotalScoreForB = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerBScoreCard),

    [PlayerAScoreCard, PlayerBScoreCard, false, false];
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
      
      {player, PlayerANode} !
        {play_request, self(), PlayerAName,
          {make_ref(), Tid, Gid, Roll, DieToA, ReplacedScoreCardA, ReplacedScoreCardB}},

      %Recieve for player A only
      receive
        {play_action, _, PlayerAName, {_, _, _, _, DiceToKeepA, ScorecardAChoice}} -> 
          %Receive for player B only
            {player, PlayerBNode} ! { play_request, 
                          self(), 
                          PlayerBName, 
                          {make_ref(), Tid, Gid, Roll, DieToB, ReplacedScoreCardB, ReplacedScoreCardA}},
          receive
            {play_action, _, PlayerBName, {_, _, _, _, DiceToKeepB, ScorecardBChoice}} -> 

              %So do not care about any of the score update logic until roll 3
              if Roll == 3 -> 
                ValueAtScoreCardRowForA = lists:nth(ScorecardAChoice, PlayerAScoreCard),
                ValueAtScoreCardRowForB = lists:nth(ScorecardBChoice, PlayerBScoreCard),
                %check if the slots are already taken

                if 
                  ValueAtScoreCardRowForA =/= -1 ->
                    printnameln("~s cheated", [PlayerAName]);
                  true  ->
                    printnameln("~s has a valid move", [PlayerAName])
                end,

                NewPlayerAScore = score_logic(ScorecardAChoice,
                  lists:nth(ScorecardAChoice, PlayerAScoreCard), DieToA),

                %Mark A's score card
                NewScorecardA = element(1, lists:split(ScorecardAChoice - 1, PlayerAScoreCard)) ++ 
                  [NewPlayerAScore] ++ 
                  element(2, lists:split(ScorecardAChoice, PlayerAScoreCard)),

                printnameln("~s's scorecard is: ~p", [PlayerAName, NewScorecardA]),
                printnameln("~s's choice is: ~p", [PlayerAName, ScorecardAChoice]),
                printnameln("~s's die is: ~p", [PlayerAName, DieToA]),
                printnameln("~s scores: ~p", [PlayerAName, NewPlayerAScore]),


                %check if the slots are already taken
                if ValueAtScoreCardRowForB =/= -1 
                      -> printnameln("~s cheated", [PlayerBName]);
                  true  -> printnameln("~s has a valid move", [PlayerBName])
                end,

                NewPlayerBScore = score_logic(ScorecardBChoice,
                  lists:nth(ScorecardBChoice, PlayerBScoreCard), DieToB),

                NewScorecardB = element(1, lists:split(ScorecardBChoice-1, PlayerBScoreCard)) ++ 
                                  [NewPlayerBScore] ++ 
                                element(2, lists:split(ScorecardBChoice, PlayerBScoreCard)),

                printnameln("~s's scorecard is: ~p", [PlayerBName, NewScorecardB]),
                printnameln("~s's choice is: ~p", [PlayerBName, ScorecardBChoice]),
                printnameln("~s's die is: ~p", [PlayerBName, DieToB]),
                printnameln("~s scores: ~p", [PlayerBName, NewPlayerBScore]),

                _TotalScoreForA = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerAScoreCard),
                _TotalScoreForB = lists:foldl(fun(X, Accin) -> Accin+X end, 0, PlayerBScoreCard),

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
            after ?TIMEOUT -> 
              %Since A is still responding and B timedout, A should win!
               ScorecardA = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
               ScorecardB = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
               %Add more to scorecard B since he/she won!
               IncrementPlayerAScoreBy = 1000,
               NewScorecardA = element(1, lists:split(?BONUSINDEX-1, ScorecardA)) ++ 
                              [IncrementPlayerAScoreBy] ++ 
                              element(2, lists:split(?BONUSINDEX, ScorecardA)),
                [NewScorecardA, ScorecardB, false, true]
          end;
        InvalidMessage -> printnameln("Invalid message: ~p", [InvalidMessage])

      after ?TIMEOUT -> 
          printnameln("~s timed out", [PlayerAName]),

          %send message to player B
            {player, PlayerBNode} ! {play_request, 
              self(), 
              PlayerBName, 
              {make_ref(), Tid, Gid, Roll, DieToB, ReplacedScoreCardB, ReplacedScoreCardA}
            },
            receive
              {play_action, _, PlayerBName, {_, _, _, _, _DiceToKeepB, _ScorecardBChoice}} -> 
                %Since player B reacted, he/she automatically wins
                    ScorecardA = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
                    ScorecardB = generate_fixed_length_lists("scorecard", ?SCORECARDROWS),
                    %Add more to scorecard B since he/she won!
                IncrementPlayerBScoreBy = 1000,
                    NewScorecardB = element(1, lists:split(?BONUSINDEX-1, ScorecardB)) ++ 
                                [IncrementPlayerBScoreBy] ++ 
                                element(2, lists:split(?BONUSINDEX, ScorecardB)),
                        [ScorecardA, NewScorecardB, false, true];
                _ -> printnameln("Not sure what to do here...")
            after ?TIMEOUT -> 
                  printnameln("~s timed out", [PlayerBName]),
                  [ReplacedScoreCardA, ReplacedScoreCardB, true, false]
            end
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
        _NewScoreCard = element(1, lists:split(?BONUSINDEX-1, Scorecard)) ++ 
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
      _Problem = lists:nth(NextIndex, DieSequence),
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
