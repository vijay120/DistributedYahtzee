set -o verbose
erlc player_controller.erl
erlc yahtzee_manager.erl
erlc tournament_manager.erl
erlc referee.erl
erlc yahtzee_player1.erl
set +o verbose
# echo "==== SLEEP FOR 3 SECONDS ^-- see above compile results to see if there are any errors ================"
# sleep 3
echo "========================================================================="
nodename=$1
username=$2
password=$3
system_full_name=$4
full_node_name=$nodename@$(hostname -s)
echo "nodename = "$nodename
echo "full_node_name = "$full_node_name
echo "username = "$username
echo "password = "$password
echo "system_full_name = "$system_full_name
echo "========================================================================="
set -o verbose
erl -noshell -run yahtzee_player1 main $nodename $username $password $system_full_name
set +o verbose