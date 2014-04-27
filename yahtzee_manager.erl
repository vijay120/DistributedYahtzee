%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(yahtzee_manager).

%% Example:

% {ash:1} erl -noshell -run yahtzee_manager main system_manager_name

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
  NodeName = list_to_atom(hd(Params)),
  % IMPORTANT: Start the epmd daemon!
  os:cmd("epmd -daemon"),
  % format microseconds of timestamp to get an
  % effectively-unique node name
  case net_kernel:start([NodeName, shortnames]) of
    {ok, _Pid} ->
      printnameln("kernel started successfully with the shortnames ~p", [NodeName]);
    {error, TheReason} ->
      printnameln("Fail to start kernel! intended shortnames: ~p,", [NodeName]),
      printnameln("Reason: ~p", [TheReason])
  end,
  register(NodeName, self()),
  printnameln("Registered with the process name = ~p, nodename = ~p",
    [NodeName, node()]),
  process_start().


%% ====================================================================
%%                   System -> Referee Functions
%% ====================================================================

process_start() ->
  printnameln("Spawning the first referee process..."),
  % spawn's arguments: Module, Function, Args
  % Pid = spawn(referee, referee_main, [["the_head_ref"]]),
  % printnameln("Referee process spawned! Its PID is ~p", [Pid]),
  listen().

% TODO: Add data structure here.
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
            printnameln("request_tournament message received from ~p with " ++
                "num-players = ~p, games-per-match = ~p.",
                [Pid, NumPlayers, GamePerMatch]),
            Tid = ?TEMP,
            Players = [?TEMP, ?TEMP],
            OptionalData = [],
            % The following message types can be sent from the rest of the system to
            % a player:
            %
            %    > start_tournament - data is a single tid, a tournament identifier
            % used (optionally) by the player to keep track of tournaments in which
            % it is participating; this message must be sent to a player for a
            % tournament x, and a positive response must be received from the
            % player, before any subsequent gameplay messages for tournament x are
            % sent to that player
            SamplePlayerPid = self(), % <----- ?TEMP,
            SamplePlayerPid ! {start_tournament, self(), {Tid}},
            % wait until all players accept tournament......
            % TODO: pass in Pid of the tournament requester
            % TEMP: Send tournament_started before the loop. It should actually come after loop.
            Pid ! {tournament_started, Pid, {Tid, Players, OptionalData}},
            listen(),
            %
            % TODO: These lines won't actually happen. They need
            % to happen when all the players accept the request.
            %   +++++++++++ play_request DELEGATED TO REFEREE +++++++++++
            %   >  play_request - data is a tuple { ref, tid, gid, roll-number, dice,
            % scorecard, opponent-scorecard }.
            %       >> ref is a unique ref identifying this play request;
            %       >> tid is the tournament identifier of a tournament in
            %          which the player is currently playing; gid is a game identifier for
            %          the game in which the player is being asked to play;
            %       >> roll-number is the roll number on the current turn (1, 2, 3)
            %          the player is playing;
            %       >> dice is a list of 5 die values (1-6) that represent the dice as they
            %          appear on the current turn; scorecard is the player's scorecard as of
            %          the completion of the previous round of the current game; and
            %       >> opponent-scorecard is the opponent's scorecard as of the completion of
            %          the previous round of the current game. The scorecard and opponent-
            %          scorecard values are here with the assumption that we want to send
            %          this information around;
            %
            %   > end_tournament - data is a single tid that the player has
            % previously received in a start_tournament message; this message is
            % sent to a player for a tournament x when that player has no more games
            % to play in tournament x, and does not convey a result.
            SamplePlayerPid ! {end_tournament, self(), {Tid}};
            
        % tournament-info - data is a tournament ID 
        {tournament_info, Pid, {TournamentId}} ->
            printnameln("tournament_info message received from ~p with " ++
                "tournament ID = ~p.", [Pid, TournamentId]),
            Status = ?TEMP,
            Winner = ?TEMP,
            OptionalData = [],
            Pid ! {tournament_status, Pid,
                {TournamentId, Status, Winner, OptionalData}};

        % user-info - data is a username
        {user_info, Pid, {Username}} ->
            printnameln("user_info message received from ~p with " ++
                "username = ~p.", [Pid, Username]),
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

        {login, Pid, _Username, {Username, Password}} ->
            % The following message types can be sent from the rest of the system to
            % a player:
            %
            %    > logged_in - data is a login-ticket, a ref that the player can use
            % to later log out in an orderly fashion. This message is sent by the
            % system to a player when the player logs in.
            printnameln("login message received from ~p with " ++
                "username = ~p, password = ~p.",
                [Pid, Username, Password]),
            LoginTicket = make_ref(),
            Pid ! {logged_in, self(), Username, {LoginTicket}},
            printnameln("logged_in message sent to ~p with " ++
                "login-ticket = ~p.", [Pid, LoginTicket]);

        % logout - data is a
        %    { login-ticket };
        %
        %     which was acquired in the most recent
        %       logged-in message.
        %     This logs the player out (making them ineligible for
        %       playing in tournaments until they log in again).

        {logout, Pid, {LoginTicket}} ->
            % TODO: Change status of the player to logout if login ticket is good.
            printnameln("logout message received from ~p with " ++
                "login-ticket = ~p.", [Pid, LoginTicket]);

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
            printnameln("accept_tournament message received from ~p with " ++
                "tid = ~p, login-ticket = ~p.", [Pid, Tid, LoginTicket]);

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
            printnameln("reject_tournament message received from ~p with " ++
                "tid = ~p, login-ticket = ~p.", [Pid, Tid, LoginTicket]);

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
        %        1-13 represent lines on the scorecard in the order presented
        %        in the homework assignment, while 0 represents no scoring for
        %        this action. Sending a non-zero value for scorecard-line ends
        %        the round for the player and scores that line in the scorecard;
        %        it is a violation of the protocol for the player to send 0 for
        %        scorecard-line on roll-number 3.
        {play_action, Pid, {Ref, Tid, Gid, RollNumber, DiceToKeep, ScorecardLine}} ->
            printnameln("play_action message received from ~p with " ++
                "ref = ~p, tid = ~p, gid = ~p",
                [Pid, Tid, Gid]),
            printnameln("roll-number = ~p, dice-to-keep = ~p, scorecard-line = ~p",
                [RollNumber, DiceToKeep, ScorecardLine]);
            % TODO: Forward to referee.


        %% ==============================================================
        %%                             Else
        %% ==============================================================
        {MessageType, Pid, Data} ->
            printnameln("Malformed ~p message from ~p with data = ~p",
                [MessageType, Pid, Data]),
            Pid ! {error, "Malformed message."}
    end,
    listen().


%% ====================================================================
%%                       Pretty Print Functions
%% ====================================================================
printnameln(ToPrint) ->
  println(io_lib:format("~s > ", [node()]) ++ ToPrint).

printnameln(ToPrint, Options) ->
  println(io_lib:format("~s > ", [node()]) ++ ToPrint, Options).


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

