%%%-------------------------------------------------------------------
%%% > c("src/basic").
%%%-------------------------------------------------------------------
-module(basic).

%% API
%% don't forget to put the functions (with their arity)
%% you want public here
-export([divide/2, sum/2, sum/1, apply/3]).

%% test 2 & 3
divide(_A, 0) -> undefined;
divide(A, B) -> A / B.

%% test 4 & 5
sum({A1, A2, A3}, {B1, B2, B3}) -> {A1 + B1, A2 + B2, A3 + B3};
sum(A, B) -> A + B.

%% test 6 ->
sum([]) -> 0;
sum([H | T]) -> sum(H, sum(T)).

%% test 8 ->
apply(A, Operator, B) ->
  case Operator of
    '+' -> sum(A, B);
    '/' -> divide(A, B);
    _ -> no_op
  end.