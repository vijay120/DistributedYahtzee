set -o verbose
erlc player_controller.erl
erlc yahtzee_manager.erl
erlc tournament_manager.erl
erlc referee.erl
erlc yahtzee_player1.erl
set +o verbose
echo "==== SLEEP FOR 3 SECONDS ^-- see above compile results to see if there are any errors ================"
sleep 3
echo "========================================================================="
nodename=$1
username=$2
full_node_name=$nodename@$(hostname -s)
echo "full_node_name = "$full_node_name
echo "username = "$username
echo "========================================================================="
set -o verbose
erl -noshell -run player_controller main $full_node_name please_logout $username
set +o verbose