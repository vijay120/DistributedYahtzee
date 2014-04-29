%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(tournament_manager).

%% ## Not supposed to be started by a command line
% args: main <pid> nodename num-players games-per-match [username]

-import(yahtzee_manager, [println/1, println/2]).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([tournament_main/1]).
%% ====================================================================
%%                             Constants
%% ====================================================================
-define(TEMP, 1).
-define(GLOBALNAME, "TournamentManager").
%% ====================================================================
%%                            Main Function
%% ====================================================================
% The main/1 function.
tournament_main(Params) ->
  % The only parameter is the name of the node to register. This
  % should be a lowercase ASCII string with no periods or @ signs.
  TournamentRequesterPid = hd(Params),
  NodeName = list_to_atom(hd(tl(Params))),
  NumPlayers = hd(tl(tl(Params))),
  GamesPerMatch = hd(tl(tl(tl(Params)))),
  Usernames = tl(tl(tl(tl(Params)))),
  % IMPORTANT: Start the epmd daemon!
  os:cmd("epmd -daemon"),
  register(NodeName, self()),
  printnameln("Registered with the process name = ~p, nodename = ~p",
    [NodeName, node()]),
  RefereeGids = [],
  OptionalData = [],
  wait_for_all_players(TournamentRequesterPid, Usernames, Usernames, OptionalData),
  play(NumPlayers, GamesPerMatch, Usernames, in_progress, RefereeGids, OptionalData).


%% ====================================================================
%%                      TM <-> Players Functions
%% ====================================================================
wait_for_all_players(TournamentRequesterPid, [], Usernames, OptionalData) ->
    Pid = TournamentRequesterPid,
    PidForReply = self(),
    Tid = self(),
    Pid ! {tournament_started, PidForReply, {Tid, Usernames, OptionalData}};
wait_for_all_players(TournamentRequesterPid, WaitingUsernames, Usernames, OptionalData) ->
  OnePlayer = 
    receive
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
              "tid = ~p, login-ticket = ~p.", [Pid, Tid, LoginTicket]),
          % TODO: Convert a login ticket to username.
          LoginTicket;

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
              "tid = ~p, login-ticket = ~p.", [Pid, Tid, LoginTicket]),
          LoginTicket;
      %% ==============================================================
      %%                             Else
      %% ==============================================================
      {MessageType, Pid, Data} ->
          printnameln("Malformed ~p message from ~p with data = ~p",
              [MessageType, Pid, Data]),
          Pid ! {error, "Malformed message."}
    end,
  wait_for_all_players(TournamentRequesterPid, WaitingUsernames -- [OnePlayer], Usernames, OptionalData).


play(NumPlayers, GamesPerMatch, Usernames, in_progress, RefereeGids, OptionalData) ->
  % TODO: Keep track when it finishes
  play(NumPlayers, GamesPerMatch, Usernames, in_progress, RefereeGids, OptionalData);
play(NumPlayers, GamesPerMatch, Usernames, completed, RefereeGids, OptionalData) ->
  play(NumPlayers, GamesPerMatch, Usernames, completed, RefereeGids, OptionalData).

%% ====================================================================
%%                       Pretty Print Functions
%% ====================================================================
getName() ->
  ?GLOBALNAME ++ io_lib:format("~p", [self()]).

printnameln(ToPrint) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint).

printnameln(ToPrint, Options) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint, Options).