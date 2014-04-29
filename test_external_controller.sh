erlc external_controller.erl
erlc yahtzee_manager.erl
erlc tournament_manager.erl
erlc referee.erl
erlc player.erl

full_node_name=node_name@$(hostname -s)
erl -noshell -run external_controller main $full_node_name request_tournament 1 1
erl -noshell -run external_controller main $full_node_name tournament_info 1
erl -noshell -run external_controller main $full_node_name user_info 1

