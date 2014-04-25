%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(distributed_yahtzee).

%% Example:

% {ash:1} erl -noshell -run distributed_yahtzee main system_manager_name

-import(refereee, [referee_main/1]).
-import(player, [player_main/1]).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([main/1, print/1, print/2, println/1, println/2]).
%% ====================================================================
%%                             Constants
%% ====================================================================
-define(TEMP, 1).
%% ====================================================================
%%                            Main Function
%% ====================================================================
% The main/1 function.
main(Params) ->
  % The only parameter is the name of the node to register. This
  % should be a lowercase ASCII string with no periods or @ signs.
  NodeName = hd(Params),
  % IMPORTANT: Start the epmd daemon!
  os:cmd("epmd -daemon"),
  % format microseconds of timestamp to get an
  % effectively-unique node name
  case net_kernel:start([list_to_atom(NodeName), shortnames]) of
    {ok, _Pid} ->
      println("kernel started successfully with the shortnames " ++ NodeName);
    {error, TheReason} ->
      println("fail to start kernel! intended shortnames: " ++ NodeName),
      println("Reason: ~p", TheReason)
  end,
  register(distributed_yahtzee, self()),
  println("~s > Registered with the process name = ~s, nodename = ~p",
    [node(), "distributed_yahtzee", node()]),
  process_start().


%% ====================================================================
%%                   System -> Referee Functions
%% ====================================================================

process_start() ->
  println("~s > Spawning the first referee process with...", [node()]),
  listen().
  % spawn's arguments: Module, Function, Args
  % referee's arguments: ???? TODO ????
  % Pid = spawn(referee, referee_main, []),
  % println("~s > Referee process ~p spawned! Its PID is ~p", [node(), Id, Pid]).

listen() ->
    receive
        %% =============================================================
        %%                System <-> External World Functions
        %% =============================================================
        {request_tournament, Pid, {NumPlayer, GamePerMatch}} ->
            Tid = ?TEMP,
            Players = [?TEMP, ?TEMP],
            OptionalData = [],
            Pid ! {tournament_started, Pid, {Tid, Players, OptionalData}};
            
        {tournament_info, Pid, {TournamentId}} ->
            Status = ?TEMP,
            Winner = ?TEMP,
            OptionalData = [],
            Pid ! {tournament_status, Pid, {TournamentId, Status, Winner, OptionalData}};

        {user_info, Pid, {Username}} ->
            Username = ?TEMP,
            MatchWins = [?TEMP, ?TEMP],
            MatchLosses = [?TEMP, ?TEMP],
            TournamentsPlayed = [?TEMP, ?TEMP],
            TournamentWins = [?TEMP, ?TEMP],
            Pid ! {user_status, Pid, {Username, MatchWins, MatchLosses, TournamentsPlayed, TournamentWins}};
        %% ==============================================================
        %%                   System <-> Player Functions
        %% ==============================================================
        %% ==============================================================
        %%                             Else
        %% ==============================================================
        {_, Pid, _Data} ->
            Pid ! {error, "Unexpected message."}
    end,
    listen().


%% ====================================================================
%%                       Pretty Print Functions
%% ====================================================================
% Helper functions for timestamp handling.
get_two_digit_list(Number) ->
  if Number < 10 ->
       ["0"] ++ integer_to_list(Number);
     true ->
       integer_to_list(Number)
  end.
get_three_digit_list(Number) ->
  if Number < 10 ->
       ["00"] ++ integer_to_list(Number);
     Number < 100 ->
         ["0"] ++ integer_to_list(Number);
     true ->
       integer_to_list(Number)
  end.
get_formatted_time() ->
  {MegaSecs, Secs, MicroSecs} = now(),
  {{Year, Month, Date},{Hour, Minute, Second}} =
    calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
  integer_to_list(Year) ++ ["-"] ++
  get_two_digit_list(Month) ++ ["-"] ++
  get_two_digit_list(Date) ++ [" "] ++
  get_two_digit_list(Hour) ++ [":"] ++
  get_two_digit_list(Minute) ++ [":"] ++
  get_two_digit_list(Second) ++ ["."] ++
  get_three_digit_list(MicroSecs div 1000).

% println/1
% print and add a new line at the end
println(ToPrint) ->
  print(ToPrint ++ "~n").

% println/2
println(ToPrint, Options) ->
  print(ToPrint ++ "~n", Options).

% print/1
% includes system time.
print(ToPrint) ->
  io:format(get_formatted_time() ++ ": " ++ ToPrint).
% print/2
print(ToPrint, Options) ->
  io:format(get_formatted_time() ++ ": " ++ ToPrint, Options).


% pretty_print_list_of_nums/1
% The parameter is a list L. If L = [1, 2, 3], it returns "[1, 2, 3]".
pretty_print_list_of_nums(L) ->
    StringList = lists:map(fun(Num) -> integer_to_list(Num) end, L),
    "[" ++ string:join(StringList, ", ") ++ "]".

