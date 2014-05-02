% Taken from
% http://stackoverflow.com/questions/8817171/shuffling-elements-in-a-list-randomly-re-arrange-list-elements/8820501#8820501
-module(shuffle).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([shuffle/1, shuffle/2]).

shuffle(L) ->
  shuffle(list_to_tuple(L), length(L)).

shuffle(T, 0) ->
  tuple_to_list(T);

shuffle(T, Len) ->
  Rand = random:uniform(Len),
  A = element(Len, T),
  B = element(Rand, T),
  T1 = setelement(Len, T,  B),
  T2 = setelement(Rand,  T1, A),
  shuffle(T2, Len - 1).