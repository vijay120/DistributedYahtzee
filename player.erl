<<<<<<< HEAD
-module(player).
-export([main/1]).

main(Params) ->
	_ = os:cmd("epmd -daemon"),
	Reg_name = hd(Params),
	net_kernel:start([list_to_atom(Reg_name), shortnames]),
	register(player, self()),
	waitForRefereeToStartGame().


waitForRefereeToStartGame() ->
	receive
		{Pid, wait_for_referee, PlayerAPid, PlayerBPid, GameId, TournamentId} -> 
			io:format("PlayerAPid is ~p", [PlayerAPid]),
			io:format("PlayerBPid is ~p", [PlayerBPid]),
			io:format("GameID is ~p", [GameId]);
		_ -> io:format("whateves man")
	end.




=======
%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(player).

-import(distributed_yahtzee, [println/1, println/2]).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([player_main/1]).
%% ====================================================================
%%                             Constants
%% ====================================================================

%% ====================================================================
%%                            Main Function
%% ====================================================================
player_main(Params) ->
  os:cmd("epmd -daemon"),
  Reg_name = hd(Params),
  net_kernel:start([list_to_atom(Reg_name), shortnames]),
  register(player, self()),
  waitForRefereeToStartGame().


waitForRefereeToStartGame() ->
  receive
    {Pid, wait_for_referee, PlayerAPid, PlayerBPid, GameId, TournamentId} -> 
      printnameln("PlayerAPid is ~p", [PlayerAPid]),
      printnameln("PlayerBPid is ~p", [PlayerBPid]),
      printnameln("GameID is ~p", [GameId]);
    _ -> printnameln("whateves man")
  end.


printnameln(ToPrint) ->
  println(io_lib:format("~s > ", [node()]) ++ ToPrint).

printnameln(ToPrint, Options) ->
  println(io_lib:format("~s > ", [node()]) ++ ToPrint, Options).
>>>>>>> 6ae5e65bb33b5f5a37653ccb8864ce52d0af04a3
