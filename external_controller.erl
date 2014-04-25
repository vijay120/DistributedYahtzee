%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(external_controller).

% this assumes that the system manager's process name is 'distributed_yahtzee'
%% Example:

% {ash:1} erl -noshell -run external_controller main $node_name $action

% action = 
%   request_tournament NumPlayers GamePerMatch
%   tournament_info TournamentId
%   user_info Username


%% ====================================================================
-define(GlobalName, "ExternalController").
%% ====================================================================
-export([main/1]).

-import(distributed_yahtzee, [println/1, println/2]).

% The following message types can be sent from the rest of the system
% to the outside world:

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
    %        (Sent in response to a start-tournament.)
    {tournament_started, Pid, {Tid, Players, OptionalData}} ->
      printnameln("The list of players is ~p", [Players]);

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
    {tournament_status, Pid, {Tid, Status, Winner, OptionalData}} ->
      printnameln("The tournament status is ~p. The winner is ~p",
        [Status, Winner]);

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
      printnameln("The match wins is ~p", [MatchWins]),
      printnameln("The match losses is ~p", [MatchLosses]),
      printnameln("The tournaments played are ~p", [TournamentsPlayed]),
      printnameln("The tournaments wins is ~p", [TournamentWins]);

    {error, Error} ->
      printnameln("Error: ~p", [Error]);
    _ ->
      printnameln("Error: Bad response!")
  end,
  halt().


main(Params) ->
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
          GamePerMatchString = hd(tl(TheRest)),
          NumPlayers = list_to_integer(NumPlayersString),
          GamePerMatch = list_to_integer(GamePerMatchString),
          printnameln("Sending a tournament_request message with " ++
            "data = {~p, ~p}...", [NumPlayers, GamePerMatch]),
          {distributed_yahtzee, SystemManagerNode} !
            {request_tournament, self(), {NumPlayers, GamePerMatch}};
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
          {distributed_yahtzee, SystemManagerNode} !
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
          {distributed_yahtzee, SystemManagerNode} !
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
