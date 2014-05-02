erlc yahtzee_manager.erl
erlc tournament_manager.erl
erlc referee.erl
erlc yahtzee_player1.erl
erlc shuffle.erl

node_name=node_name
erl -noshell -run yahtzee_manager main $node_name
