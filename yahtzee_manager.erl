%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(yahtzee_manager).

%% Example:

% {ash:1} erl -noshell -run yahtzee_manager main system_manager_name

-import(tournament_manager, [tournament_main/1]).
-import(referee, [referee_main/1]).
-import(player, [player_main/1]).
-import(shuffle, [shuffle/1]).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([main/1, print/1, print/2, println/1, println/2]).
%% ====================================================================
%%                             Constants
%% ====================================================================
-define(TEMP, 1).
-define(TEMPSRING, "1").

% For UserTable indexing...
-define(PID, 1).
-define(NODE, 2).
-define(USERNAME, 3).
-define(PASSWORD, 4).
-define(LOGIN_TICKET, 5).
-define(IS_LOGIN, 6).
-define(MATCH_WINS, 7).
-define(MATCH_LOSSES, 8).
-define(TOURNAMENTS_PLAYED, 9).
-define(TOURNAMENTS_WIN, 10).

% For TournamentStatuses indexing...
-define(TID, 1).
-define(STATUS, 2).
-define(WINNER, 3).
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
  register(yahtzee_manager, self()),
  printnameln("Registered with the process name = ~p, nodename = ~p, pid = ~p",
    [NodeName, node(), self()]),
  TournamentStatuses = [],
  UserTables = [],
  listen(TournamentStatuses, UserTables).


listen(TournamentStatuses, UserTables) ->
  printnameln("=========================================="),
  printnameln("TournamentStatuses = ~p", [TournamentStatuses]),
  printnameln("UserTables = ~p", [UserTables]),
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
    {request_tournament, ExternalControllerPid, {NumPlayers, GamesPerMatch}} ->
        printnameln("request_tournament message received from ~p with " ++
            "num-players = ~p, games-per-match = ~p.",
            [ExternalControllerPid, NumPlayers, GamesPerMatch]),
        case ((GamesPerMatch >= 0) and (GamesPerMatch rem 2 == 1)) of
          true ->
            LoggedInPlayerList = [X || X <- UserTables, element(?IS_LOGIN, X) == true],
            printnameln("Logged-in players are ~p", [LoggedInPlayerList]),
            % Cut down to only NumPlayers
            Players = lists:sublist(shuffle(LoggedInPlayerList), NumPlayers),
            printnameln("Out of those, because NumPlayers = ~p, we select ~p",
              [NumPlayers, Players]),
            _OptionalData = [],
            
            printnameln("Spawning a tournament_manager process..."),
            Nodename = string:concat("TournamentManager",
              integer_to_list(length(TournamentStatuses) + 1)),
            printnameln("Nodename = ~p", [Nodename]),

            Tid = spawn(tournament_manager, tournament_main,
              [
                [
                  ExternalControllerPid,
                  self(),
                  Nodename, 
                  NumPlayers,
                  GamesPerMatch
                ]
                ++
                Players
              ]
            ),
            printnameln("tournament process spawned! Its TID is ~p", [Tid]),

            NewTournamentStatuses = TournamentStatuses ++ [{Tid, in_progress, undefined}], % if in_progress, ignore winner entry
            listen(NewTournamentStatuses, UserTables);
          false ->
            printnameln("Reject a non positive odd number of GamesPerMatch.")
        end;
    % tournament-info - data is a tournament ID 
    {tournament_info, Pid, {TournamentId}} ->
        printnameln("tournament_info message received from ~p with " ++
            "tournament ID = ~p.", [Pid, TournamentId]),
        printnameln("TournamentStatuses: ~p", [TournamentStatuses]),

        {Status, Winner} = 
          case lists:keyfind(TournamentId, ?TID, TournamentStatuses) of
            false ->
              printnameln("Tournament does not exist."),
              {undefined, undefined};

            {_, CaseStatus, CaseWinner} -> 
              {CaseStatus, CaseWinner};

            Badcode ->
              printnameln("Tuple of inappropriate size: ~p", [Badcode]),
              {undefined, undefined}
          end,

          OptionalData = [],

          printnameln("tournament_status message sent to ~p with ", [Pid]),
          printnameln("  tid = ~p", [TournamentId]),
          printnameln("  status = ~p", [Status]),
          printnameln("  winner = ~p", [Winner]),
          printnameln("  optional-data = ~p", [OptionalData]),
          printnameln(""),

          Pid ! {tournament_status, Pid,
                  {TournamentId, Status, Winner, OptionalData}};

    % user-info - data is a username
    {user_info, Pid, {Username}} ->
        printnameln("user_info message received from ~p with " ++
            "username = ~p.", [Pid, Username]),

        {MatchWins, MatchLosses, TournamentsPlayed, TournamentWins} = 
          case lists:keyfind(Username, ?USERNAME, UserTables) of
            false ->
              printnameln("Username does not match any existing usernames"), 
              {undefined,undefined,undefined,undefined};
            {_, _, _, _, _, _, CaseMatchWins,
              CaseMatchLosses, CaseTournamentsPlayed, CaseTournamentWins} ->
                {CaseMatchWins, CaseMatchLosses, CaseTournamentsPlayed, CaseTournamentWins}
          end,

        Pid ! {user_status, Pid,
            {Username, MatchWins, MatchLosses,
            TournamentsPlayed, TournamentWins}},
        printnameln("user_status message sent to ~p with ", [Pid]),
        printnameln("  username = ~p", [Username]),
        printnameln("  match-wins = ~p", [MatchWins]),
        printnameln("  match-losses = ~p", [MatchLosses]),
        printnameln("  tournaments-played = ~p", [TournamentsPlayed]),
        printnameln("  tournament-wins = ~p", [TournamentWins]),
        printnameln("");
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
    {login, Pid, Nodename, {Username, Password}} ->
        % The following message types can be sent from the rest of the system to
        % a player:
        %
        %    > logged_in - data is a login-ticket, a ref that the player can use
        % to later log out in an orderly fashion. This message is sent by the
        % system to a player when the player logs in.

        printnameln("login message received from ~p with " ++
            "username = ~p, password = ~p.",
            [Pid, Username, Password]),
        
        % Check if the player is in the user tables; 
        % If yes, change is_login to true. Else just add it
        % to user table with fresh stats.
        case lists:keyfind(Username, ?USERNAME, UserTables) of
          false ->
            % add to user table with fresh stats
            LoginTicket = make_ref(),
            NewRecord = {Pid, Nodename, Username, Password, LoginTicket, true, 0,0,0,0},
            NewUserTables = [NewRecord | UserTables],
            Pid ! {logged_in, self(), Username, {LoginTicket}},
            printnameln("logged_in message sent to ~p with " ++
                "login-ticket = ~p.", [Pid, LoginTicket]),
            printnameln("");

          {_, Nodename, Username, Password, LoginTicket, _, MatchWins, MatchLosses,
          TournamentsPlayed, TournamentWins} ->
            % re-login
            NewRecord = {Pid, Nodename, Username, Password, LoginTicket, true, MatchWins,
                         MatchLosses, TournamentsPlayed, TournamentWins},
            % TODO: Do lists:keyreplace instead of just insert | To Eoin, I fixed this. Can you double check? - Tum
            NewUserTables = lists:keyreplace(Username, ?USERNAME, UserTables, NewRecord),
            Pid ! {logged_in, self(), Username, {LoginTicket}},
            printnameln("logged_in message sent to ~p with " ++
                "login-ticket = ~p.", [Pid, LoginTicket]),
            printnameln("");

          {UserPid, _, Username, _,_,_,_,_,_,_} -> % without pattern matching password/node name
            LoginTicket = make_ref(),
            NewUserTables = UserTables,
            printnameln("Password does not match!"),
            exit(UserPid, badarg) % TODO: What do we do now??
        end,

        listen(TournamentStatuses, NewUserTables);

    % logout - data is a
    %    { login-ticket };
    %
    %     which was acquired in the most recent
    %       logged-in message.
    %     This logs the player out (making them ineligible for
    %       playing in tournaments until they log in again).

    {logout, Pid, Username, {LoginTicket}} ->
        % replace LoginStatus with false, update our UserTables.
        printnameln("logout message received from ~p with " ++
            "login-ticket = ~p.", [Pid, LoginTicket]),

        NewUserTables = case lists:keyfind(LoginTicket, ?LOGIN_TICKET, UserTables) of
          false -> 
            printnameln("LoginTicket does not match any existing players."),
            UserTables;
          Result -> 
            ResultUpdated = setelement(?IS_LOGIN, Result, false),
            lists:keyreplace(LoginTicket, ?LOGIN_TICKET, UserTables, ResultUpdated)
        end,
        
        listen(TournamentStatuses, NewUserTables);
          
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
    {accept_tournament, Pid, Username, {Tid, LoginTicket}} ->
        printnameln("accept_tournament message received from ~p with " ++
            "tid = ~p, login-ticket = ~p. Forwarding to TM...", [Pid, Tid, LoginTicket]),
        Tid ! {accept_tournament, Pid, Username, {Tid, LoginTicket}};

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
    {reject_tournament, Pid, Username, {Tid, LoginTicket}} ->
        printnameln("reject_tournament message received from ~p with " ++
            "tid = ~p, login-ticket = ~p. Forwarding to TM...", [Pid, Tid, LoginTicket]),
        Tid ! {reject_tournament, Pid, Username, {Tid, LoginTicket}};

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
            "  ref = ~p, tid = ~p, gid = ~p",
            [Pid, Tid, Gid]),
        printnameln("  roll-number = ~p, dice-to-keep = ~p, scorecard-line = ~p",
            [RollNumber, DiceToKeep, ScorecardLine]),
        printnameln("Forwarding request to a referee..."),
        Gid ! {play_action, Pid, {Ref, Tid, Gid, RollNumber, DiceToKeep, ScorecardLine}};

    %% ==============================================================
    %%              yahtzee_manager <-> tournament_manager
    %% ==============================================================
    % Some tournament ended and is giving us the results. 
    % UserRecords: [{Username, MatchWins, MatchLosses}] (every player in tournament)
    % Winner: Username (the winner of the tournament)
    {report_tournament_results, Pid, {Tid, UserRecords, Winner}} ->
        printnameln("report_tournament_results message received from ~p with " ++
          " UserRecords = ~p, Winner = ~p",
          [Pid, UserRecords, Winner]
        ),
        % Update usertables with new stats from all players in tournament.
        UpdatedUserTables = updateUserTables(UserRecords, UserTables),

        {WinnerPid, WinnerNodename, _, WinnerPassword, WinnerLoginTicket, WinnerIsLogin, WinnerMatchWins,
          WinnerMatchLosses, WinnerTourneysPlayed, WinnerTourneysWon} = 
          lists:keyfind(Winner, ?USERNAME, UpdatedUserTables), % update winner with tourney win separately

        NewWinnerRecord = {
          WinnerPid, WinnerNodename, Winner, WinnerPassword,
          WinnerLoginTicket, WinnerIsLogin, WinnerMatchWins,
          WinnerMatchLosses, WinnerTourneysPlayed,
          WinnerTourneysWon + 1
        },

        ThisTournamentStatus = lists:keyfind(Tid, ?TID, TournamentStatuses),
        NewTournamentStatus = setelement(?STATUS, ThisTournamentStatus, completed),
        NewTournamentStatuses = lists:keyreplace(Tid, ?TID, TournamentStatuses, NewTournamentStatus),

        NewUserTables = lists:keyreplace(Winner, ?USERNAME, UpdatedUserTables, NewWinnerRecord),
        listen(NewTournamentStatuses, NewUserTables);

    %% ==============================================================
    %%                             Else
    %% ==============================================================
    {MessageType, Pid, Data} ->
        printnameln("Malformed ~p message from ~p with data = ~p",
            [MessageType, Pid, Data]),
        Pid ! {error, "Malformed message."};
    InvalidMessage ->
        printnameln("Invalid Message: ~p", [InvalidMessage])
  end,
  listen(TournamentStatuses, UserTables).

%% ====================================================================
%%                           Utility Functions
%% ====================================================================

updateUserTables(UserRecords, UserTables) ->
  if
    UserRecords == [] ->
      UserTables;
    true ->
      {Username, MatchWins, MatchLosses} = hd(UserRecords),

      % get existing record for player
      {Pid, Nodename, _, Password, LoginTicket, IsLogin,
        OldMatchWins, OldMatchLosses, TourneysPlayed, TourneysWon}
        = lists:keyfind(Username, ?USERNAME, UserTables),

      NewRecord = {
        Pid, Nodename, Username, Password, LoginTicket, IsLogin,
        OldMatchWins + MatchWins,
        OldMatchLosses + MatchLosses,
        TourneysPlayed + 1,
        TourneysWon
      }, % update with new stats
      NewUserTables = lists:keyreplace(Username, ?USERNAME, UserTables, NewRecord),
      updateUserTables(tl(UserRecords), NewUserTables)
  end.

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
% pretty_print_list_of_nums(L) ->
%     StringList = lists:map(fun(Num) -> integer_to_list(Num) end, L),
%     "[" ++ string:join(StringList, ", ") ++ "]".

