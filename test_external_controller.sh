set -o verbose
erlc external_controller.erl
erlc yahtzee_manager.erl
erlc tournament_manager.erl
erlc referee.erl
erlc yahtzee_player1.erl
erlc yahtzee_player2.erl
set +o verbose
# echo "==== SLEEP FOR 3 SECONDS ^-- see above compile results to see if there are any errors ================"
# sleep 3
echo "========================================================================="
nodename=$1
full_node_name=$nodename@$(hostname -s)
echo "full_node_name="$full_node_name
echo "========================================================================="
set -o verbose
erl -noshell -run external_controller main $full_node_name request_tournament 4 3
# erl -noshell -run external_controller main $full_node_name request_tournament 2 3
erl -noshell -run external_controller main $full_node_name tournament_info 1
erl -noshell -run external_controller main $full_node_name tournament_info 2
erl -noshell -run external_controller main $full_node_name user_info 1
set +o verbose