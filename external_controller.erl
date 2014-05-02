%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(external_controller).

%% Example:

% {ash:1} erl -noshell -run external_controller main $full_node_name $action

% e.g. full_node_name=node@host
%
% action = 
%   request_tournament NumPlayers GamesPerMatch
%   tournament_info TournamentId
%   user_info Username


%% ====================================================================
-define(GlobalName, "ExternalController").
%% ====================================================================
-export([main/1]).

-import(yahtzee_manager, [println/1, println/2]).

% loop() ->
%   loop_once(),
%   loop().

loop_once() ->
  receive
    % tournament-started - data is a tuple
    %     { tid, players, optional-data } where
    %
    %     > tid is the tournament ID,
    %
    %     > players is a list of the usernames assigned to play in the
    %        tournament, and optional-data is optional implementation-dependent
    %        data about the tournament (such as a representation of the bracket).
    %        (Sent in response to a request_tournament.)
    {tournament_started, Pid, {Tid, Players, _OptionalData}} ->
      printnameln("Received a tournament_started message from ~p with tid = ~p",
        [Pid, Tid]),
      printnameln("The list of players is ~p", [Players]),
      printnameln("");

    % tournament-status - data is a tuple
    %     { tid, status, winner, optional-data } where
    %
    %     > tid is the tournament ID,
    %
    %     > status is an atom indicating the tournament status (in-progress, complete),
    %     winner is
    %         the username of the winner, if a winner has been determined, or
    %         undefined if not, and optional-data is optional implementation-dependent
    %           data about the tournament (such as a representation of the bracket).
    % (Sent in response to a tournament-info.)
    % A tournament-info requesting information about an invalid tournament ID is ignored.
    {tournament_status, Pid, {Tid, Status, Winner, _OptionalData}} ->
      printnameln("Received a tournament_status message from ~p with tid = ~p",
        [Pid, Tid]),
      printnameln("The tournament status is ~p. The winner is ~p",
        [Status, Winner]),
      printnameln("");

    % user-status - data is a tuple
    %   { username, match-wins, match-losses, tournaments-played, tournament-wins } where
    %
    %   > username is a username and the other components are all integers
    %     and mostly self-explanatory;
    %
    %   > tournaments-played should include tournaments in progress that
    %     the user has agreed to participate in, since once a user is in a
    %     tournament a result will be recorded for them whether or not their
    %     player crashes. Sent in response to a user-info. A user-info requesting
    %     information about an invalid username is ignored.
    {user_status, Pid, {Username, MatchWins, MatchLosses,
      TournamentsPlayed, TournamentWins}} ->
       printnameln("Received a user_status message from ~p with username = ~p",
        [Pid, Username]),
      printnameln("The match wins is ~p", [MatchWins]),
      printnameln("The match losses is ~p", [MatchLosses]),
      printnameln("The tournaments played are ~p", [TournamentsPlayed]),
      printnameln("The tournaments wins is ~p", [TournamentWins]),
      printnameln("");

    {error, Error} ->
      printnameln("Error: ~p", [Error]);
    _ ->
      printnameln("Error: Bad response!"),
      printnameln("")
  end,
  halt().


main(Params) ->
  NodeName = yahtzee_manager,
  SystemManagerNode = list_to_atom(hd(Params)),
  Action = hd(tl(Params)),
  TheRest = tl(tl(Params)),
  net_kernel:start([external_controller, shortnames]),

  % ======== START boilerplate code to connect to global registry =========
  % case net_kernel:connect(list_to_atom(SystemManagerNode)) of
  %   true ->
  %     printnameln("Connected to ~p successfully.", [SystemManagerNode]);
  %   false ->
  %     printnameln("Error: Cannot connect to ~p! Please try again", [SystemManagerNode]),
  %     halt()
  % end,
  % ======== END boilerplate code to connect to global registry =========

  % The following message types can be sent from the outside world to
  % the rest of the system:

  %     request-tournament - data is a tuple
  %       { num-players, games-per-match };
  %
  %       > num-players is the number of players that should
  %         participate in the tournament.
  %
  %       > games-per-match is the (maximum) number of games
  %         to play for match  e.g., for a tournament with
  %         best-of-5 matches, games-per-match would be 5). 
  %
  %     tournament-info - data is a tournament ID 
  %
  %     user-info - data is a username
  case Action of
    "request_tournament" ->
      case length(TheRest) of
        2 ->
          NumPlayersString = hd(TheRest),
          GamesPerMatchString = hd(tl(TheRest)),
          NumPlayers = list_to_integer(NumPlayersString),
          GamesPerMatch = list_to_integer(GamesPerMatchString),
          printnameln("Sending a tournament_request message with " ++
            "data = {~p, ~p}...", [NumPlayers, GamesPerMatch]),
          {NodeName, SystemManagerNode} !
            {request_tournament, self(), {NumPlayers, GamesPerMatch}};
        _ ->
          halt("Error: There should be exactly two parameters after " ++
            "the keyword 'request_tournament'")
      end;

    "tournament_info" ->
      case length(TheRest) of
        1 ->
          TournamentId = hd(TheRest),
           printnameln("Sending a tournament_info message with " ++
            "data = {~p}...", [TournamentId]),
          {NodeName, SystemManagerNode} !
            {tournament_info, self(), {TournamentId}};
        _ ->
          halt("Error: There should be exactly one parameter after " ++
            "the keyword 'tournament_info'")
      end;

    "user_info" ->
      case length(TheRest) of
        1 ->
          Username = hd(TheRest),
          printnameln("Sending a tournament_request message with " ++
            "data = {~p}...", [Username]),
          {NodeName, SystemManagerNode} !
            {user_info, self(), {Username}};
        _ ->
          halt("Error: There should be exactly one parameter after " ++
            "the keyword 'user_info'")
      end;

    _ ->
      halt("Error: Not a valid request.")
  end,
  % if you want an infinite loop, use loop() instead.
  loop_once().


%% ====================================================================
%%                       Helper Functions
%% ====================================================================
printnameln(ToPrint) ->
  println(io_lib:format("~s > ", [?GlobalName]) ++ ToPrint).

printnameln(ToPrint, Options) ->
  println(io_lib:format("~s > ", [?GlobalName]) ++ ToPrint, Options).
