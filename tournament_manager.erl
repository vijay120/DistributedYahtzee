%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(tournament_manager).

%% ## Not supposed to be started by a command line
% args: tournament_main <eid> <tid> nodename num-players games-per-match [username]

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
%% ====================================================================
%%                            Main Function
%% ====================================================================
% The tournament_main/1 function.
tournament_main(Params) ->
  % The only parameter is the name of the node to register. This
  % should be a lowercase ASCII string with no periods or @ signs.
  ExternalControllerPid = hd(Params),        % <eid>
  YahtzeeManagerPid = hd(tl(Params)),        % <yid>
  NodeName = list_to_atom(hd(tl(tl(Params)))),
  NumPlayers = hd(tl(tl(tl(Params)))),
  GamesPerMatch = hd(tl(tl(tl(tl(Params))))),
  Players = tl(tl(tl(tl(tl(Params))))),
  Usernames = lists:map(fun(Player) -> element(?USERNAME, Player) end, Players),
  % IMPORTANT: Start the epmd daemon!
  os:cmd("epmd -daemon"),
  register(NodeName, self()),
  printnameln("Registered with the process name = ~p, nodename = ~p, pid = ~p",
    [NodeName, node(), self()]),
  RefereeGids = [],
  OptionalData = [],
  Tid = self(),
  ask_each_player_to_join_tournament(YahtzeeManagerPid, Tid, Players),
  wait_for_all_players(ExternalControllerPid, Usernames, Usernames, OptionalData),
  play(NumPlayers, GamesPerMatch, Usernames, in_progress, RefereeGids, OptionalData).


%% ====================================================================
%%                      TM <-> Players Functions
%% ====================================================================


ask_each_player_to_join_tournament(YahtzeeManagerPid, Tid, Players) ->
  UserPids = lists:map(fun(Player) -> element(?PID, Player) end, Players),
  Usernames = lists:map(fun(Player) -> element(?USERNAME, Player) end, Players),
  PidsUsernames = lists:zip(UserPids, Usernames),
  printnameln("The list of users is ~p.", [Usernames]),
  lists:map(
    fun({UserPid, Username}) ->
      UserPid ! {start_tournament, YahtzeeManagerPid, Username, {Tid}},
      printnameln("Send a start_tournament request to user ~p at ~p to join the tournament ~p.",
        [Username, UserPid, Tid])
    end,
    PidsUsernames).

% This is the case when all players reply.
wait_for_all_players(ExternalControllerPid, [], Usernames, OptionalData) ->
    PidForReply = self(),
    Tid = self(),
    ExternalControllerPid ! {tournament_started, PidForReply, {Tid, Usernames, OptionalData}};
wait_for_all_players(ExternalControllerPid, WaitingUsernames, Usernames, OptionalData) ->
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
  wait_for_all_players(ExternalControllerPid, WaitingUsernames -- [OnePlayer], Usernames, OptionalData).


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

% printnameln(ToPrint) ->
%   println(io_lib:format("~s > ", [getName()]) ++ ToPrint).

printnameln(ToPrint, Options) ->
  println(io_lib:format("~s > ", [getName()]) ++ ToPrint, Options).