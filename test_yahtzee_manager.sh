erlc yahtzee_manager.erl
erlc tournament_manager.erl
erlc referee.erl
erlc player.erl

node_name=node_name
erl -noshell -run yahtzee_manager main $node_name
