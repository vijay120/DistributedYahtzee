erlc referee.erl
erlc yahtzee_manager.erl
erlc player.erl
erlc externall_controller.erl

node_name=node_name
full_node_name=$node_name@$(hostname -s)
erl -noshell -run external_controller main $node_name $full_node_name request_tournament 1 1
erl -noshell -run external_controller main $node_name $full_node_name tournament_info 1
erl -noshell -run external_controller main $node_name $full_node_name user_info 1