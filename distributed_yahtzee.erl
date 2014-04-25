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
        % request-tournament - data is a tuple
        %   { num-players, games-per-match };
        %
        %   > num-players is the number of players that should
        %       participate in the tournament.
        %
        %   > games-per-match is the (maximum) number of games to play for
        %       match e.g., for a tournament with best-of-5 matches,
        %       games-per-match would be 5). A request_tournament message
        %       with an invalid games-per-match value (such as an even
        %       number, or a negative number) is ignored.
        {request_tournament, Pid, {NumPlayers, GamePerMatch}} ->
            println("~s > request_tournament message received from ~p with " ++
                "num-players = ~p, games-per-match = ~p.",
                [node(), Pid, NumPlayers, GamePerMatch]),
            Tid = ?TEMP,
            Players = [?TEMP, ?TEMP],
            OptionalData = [],
            Pid ! {tournament_started, Pid, {Tid, Players, OptionalData}};
            
        % tournament-info - data is a tournament ID 
        {tournament_info, Pid, {TournamentId}} ->
            println("~s > tournament_info message received from ~p with " ++
                "tournament ID = ~p.", [node(), Pid, TournamentId]),
            Status = ?TEMP,
            Winner = ?TEMP,
            OptionalData = [],
            Pid ! {tournament_status, Pid,
                {TournamentId, Status, Winner, OptionalData}};

        % user-info - data is a username
        {user_info, Pid, {Username}} ->
            println("~s > user_info message received from ~p with " ++
                "username = ~p.", [node(), Pid, Username]),
            MatchWins = [?TEMP, ?TEMP],
            MatchLosses = [?TEMP, ?TEMP],
            TournamentsPlayed = [?TEMP, ?TEMP],
            TournamentWins = [?TEMP, ?TEMP],
            Pid ! {user_status, Pid,
                {Username, MatchWins, MatchLosses,
                TournamentsPlayed, TournamentWins}};
        %% ==============================================================
        %%                   System <-> Player Functions
        %% ==============================================================

        % The following message types can be sent from a player to the
        % rest of the system:

        % login - data is a tuple
        %     { username, password }.
        %
        %     > username and password are strings with which the player
        %         wants to log in for tournament play. If a username was
        %         not previously registered with the system, it is registered
        %         when the first login is received for that username.
        %         There is no mechanism for changing passwords.

        {login, Pid, {Username, Password}} ->
            println("~s > login message received from ~p with " ++
                "username = ~p, password = ~p.",
                [node(), Pid, Username, Password]);

        % logout - data is a
        %    { login-ticket };
        %
        %     which was acquired in the most recent
        %       logged-in message.
        %     This logs the player out (making them ineligible for
        %       playing in tournaments until they log in again).

        {logout, Pid, {LoginTicket}} ->
            println("~s > logout message received from ~p with " ++
                "login-ticket = ~p.", [node(), Pid, LoginTicket]);

        % accept_tournament - data is a tuple
        %     { tid, login-ticket };
        %
        %     > tid is the tournament ID that the player received in
        %        a start-tournament message, and
        %
        %     > login-ticket is the login ticket the player received
        %        on its current login. This message indicates that the
        %        player is willing to play in the specified tournament.
        %
        %     > The pid-for-reply in this message determines where
        %         play-request messages for this player in the specified
        %         tournament are sent.
        {accept_tournament, Pid, {Tid, LoginTicket}} ->
            println("~s > accept_tournament message received from ~p with " ++
                "tid = ~p, login-ticket = ~p.", [node(), Pid, Tid, LoginTicket]);

        % reject_tournament - data is a tuple
        %     { tid, login-ticket };
        %
        %     > tid is the tournament ID that the player received in a
        %       start-tournament message, and
        %
        %     > login-ticket is the login ticket the player received on
        %         its current login. This message indicates that the player
        %         is not willing to play in the specified tournament.
        %         These messages are optional - if a player does not send a reply
        %         (or sends a malformed reply, such as one with a bad login-ticket)
        %         to a start_tournament message, it is assumed to have rejected
        %         the tournament; thus, tournament managers must use timeouts
        %         when setting up tournaments.
        {reject_tournament, Pid, {Tid, LoginTicket}} ->
            println("~s > reject_tournament message received from ~p with " ++
                "tid = ~p, login-ticket = ~p.", [node(), Pid, Tid, LoginTicket]);

        % play_action - data is a tuple
        %     { ref, tid, gid, roll-number, dice-to-keep, scorecard-line }.
        %
        %     This message is sent in response to a play_request message:
        %     > ref, tid, gid and roll-number are the same values received
        %        in the play_request message;
        %
        %     > dice-to-keep is a list of 5 booleans (true or false atoms)
        %         representing whether to keep each of the dice in the dice
        %         list from the play_request message; and
        %
        %     > scorecard-line is an integer representing a line on the
        %        scorecard in which to score the dice.
        %
        %     > 1-13 represent lines on the scorecard in the order presented
        %        in the homework assignment, while 0 represents no scoring for
        %        this action. Sending a non-zero value for scorecard-line ends
        %        the round for the player and scores that line in the scorecard;
        %        it is a violation of the protocol for the player to send 0 for
        %        scorecard-line on roll-number 3.
        {play_action, Pid, {Ref, Tid, Gid, RollNumber, DiceToKeep, ScorecardLine}} ->
            println("~s > play_action message received from ~p with " ++
                "ref = ~p, tid = ~p, gid = ~p",
                [node(), Pid, Tid, Gid]),
            println("~s > roll-number = ~p, dice-to-keep = ~p, scorecard-line = ~p",
                [node(), RollNumber, DiceToKeep, ScorecardLine]);


        %% ==============================================================
        %%                             Else
        %% ==============================================================
        {MessageType, Pid, Data} ->
            println("~s > Malformed ~p message from ~p with data = ~p",
                [node(), MessageType, Pid, Data]),
            Pid ! {error, "Malformed message."}
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

