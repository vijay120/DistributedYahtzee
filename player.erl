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




