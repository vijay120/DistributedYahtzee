Names: Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan

NOTE: yahtzee_player1.erl is intelligent, yahtzee_player2.erl is
dumb but quicker to run if you want to test a larger tournament.
If you are inserting us in a competition with other players,
please pick yahtzee_player1. Or both. My money's on player1 though.

See Testing.yaml for further instructions on how you can test our code.
See data_structures.yaml for information on our internal protocol.

Reflections:

This assignment was longer than expected, and we expected it to be long.
There were just a lot of different things to implement, a lot of small
errors, bugs, inconsistencies, etc. that took a long time to debug,
especially because the end result is so large.

We liked the fact that we as a class created the protocol and we like
the way we split the work between different types of processes 
(meaning our refs, players, tournament manager, and system manager).

We could've done with more time (or it being at a better time of year),
especially for organizing the code/coordinating everything between
people. 

Testing:


	Testing Referee-Player interactions:

		1) Run the following for a normal (no crashes by either player) match between two players: Gets expected behaviour
			MachineA (hollyhock): bash test_yahtzee_manager.sh node_name
			MachineB: bash test_yahtzee_player1.sh vijay1 enugent1 hunter1 node_name@hollyhock
			MachineC: bash test_yahtzee_player1.sh vijay2 enugent2 hunter2 node_name@hollyhock
			MachineD:
						full_node_name=node_name@hollyhock <- change this to machineA's host name whenever
						erl -noshell -run external_controller main $full_node_name request_tournament 2 5


		Results in Referee:

		(notice Game Done! at the end)
			https://gist.github.com/vijay120/5b884488222c45981b1d


		2) Do the following for one player crashing: Gets expected behaviour
			MachineB: ctrl-c

		Results in Referee:

		(notice Player B winning!)
			https://gist.github.com/vijay120/cbfeae01df15ff39068f


		3) Do the following for both players crashing: Gets expected behaviour

		Results in Referee:

		(notice bye! in the end)
			https://gist.github.com/vijay120/748c2fcf45cc7315ae72

		4) Test that crashing (by Ctrl + C) will automatically update the status of the player to logout.

		Results in the System Manager:
			(notice the true (indicating the player is logged-in) changes to false)
			https://gist.github.com/tummykung/864acbfae66a62dce47a





	bash test_yahtzee_manager.sh node_name
	bash test_external_controller.sh node_name
	bash test_yahtzee_player.sh  2  vijay1 enugent1 hunter1 node_name@$(hostname -s)
	bash test_yahtzee_player.sh  2  vijay2 enugent2 hunter2 node_name@$(hostname -s)
	bash test_external_controller.sh
	bash test_player_controller.sh  vijay1 enugent1

Notes:
After running test_external_controller, make sure it halts.
After running test_player_controller, make sure the corresopnding player logs out.
