set -o verbose
erlc yahtzee_manager.erl
erlc tournament_manager.erl
erlc referee.erl
erlc yahtzee_player1.erl
erlc shuffle.erl
set +o verbose
nodename=$1
echo "========================================================================="
full_node_name=$nodename@$(hostname -s)
echo "nodename = "$nodename
echo "full_node_name = "$full_node_name
echo "========================================================================="

set -o verbose
erl -noshell -run yahtzee_manager main $nodename
set +o verbose