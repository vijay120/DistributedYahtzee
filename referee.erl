%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(referee).

-import(yahtzee_manager, [println/1, println/2]).
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
