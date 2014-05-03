set -o verbose
erlc player_controller.erl
erlc yahtzee_manager.erl
erlc tournament_manager.erl
erlc referee.erl
erlc yahtzee_player1.erl
erlc yahtzee_player2.erl
set +o verbose
# echo "==== SLEEP FOR 3 SECONDS ^-- see above compile results to see if there are any errors ================"
# sleep 3
echo "========================================================================="
player_number=$1
nodename=$2
username=$3
password=$4
system_full_name=$5
full_node_name=$nodename@$(hostname -s)
echo "player_number="$player_number
echo "nodename="$nodename
echo "full_node_name="$full_node_name
echo "username="$username
echo "password="$password
echo "system_full_name="$system_full_name
echo "========================================================================="
set -o verbose
erl -noshell -run yahtzee_player$player_number main $nodename $username $password $system_full_name
# case $player_number in
#  1)
#   erl -noshell -run yahtzee_player1 main $nodename $username $password $system_full_name
#   ;;
#  2)
#   erl -noshell -run yahtzee_player2 main $nodename $username $password $system_full_name
#   ;;
#  *)
#   echo "No specified player number."
#   ;;
# esac
set +o verbose