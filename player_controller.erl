%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed Yahtzee
%% @author Tum Chaturapruek, Eoin Nugent, Vijay Ramakrishnan
-module(player_controller).

%% Example:

% {ash:1} erl -noshell -run player_controller main $full_node_name $usename $action

% e.g. full_node_name=node@host
%
% action = 
%   please_logout


%% ====================================================================
-define(GlobalName, "PlayerController").
%% ====================================================================
-export([main/1]).

-import(yahtzee_manager, [println/1, println/2]).

% loop() ->
%   loop_once(),
%   loop().

loop_once() ->
  receive
    {logged_out, Pid, Username, {}} ->
       printnameln("Received a logged_out message from ~p with username = ~p",
        [Pid, Username]),
      printnameln("");

    {error, Error} ->
      printnameln("Error: ~p", [Error]);
    _ ->
      printnameln("Error: Bad response!"),
      printnameln("")
  end,
  halt().


main(Params) ->
  NodeName = player,
  PlayerNode = list_to_atom(hd(Params)),
  Action = hd(tl(Params)),
  TheRest = tl(tl(Params)),
  net_kernel:start([player_controller, shortnames]),

  case Action of
    "please_logout" ->
      case length(TheRest) of
        1 ->
          Username = hd(TheRest),
          printnameln("Sending a please_logout message"),
          {NodeName, PlayerNode} !
            {please_logout, self(), Username, {}};
        _ ->
          halt("Error: There should be no parameters after " ++
            "the keyword 'please_logout'")
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
