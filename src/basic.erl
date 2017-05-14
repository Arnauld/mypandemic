%%%-------------------------------------------------------------------
%%% > c("src/basic").
%%%-------------------------------------------------------------------
-module(basic).

%% API
%% don't forget to put the functions (with their arity)
%% you want public here
-export([divide/2]).

%% test 2 & 3 -> edit this function
divide(A, B) -> 3.

%% test 4 & 5 -> edit this function
%%
%% hint 1 : sum of 2 vectors
%% A = (A1,A2,A3) and
%% B = (B1,B2,B3) equals
%% (A1+B1,A2+B2,A3+B3)
%%
%% hint 2 : is this function public ?
sum(A, B) -> A + B.

%% test 6 ->
%% is sum(List) really the same function as previous ?
%% hint : to do recursion
%% 1) think about the terminal clause first
%% 2) the last definition is the one containing call to itself
%% 3) tail-call optimisation recursion :
%% when the last operation of a function is to call itself,
%% we can get rid of the callstack (ever heard of stackoverflow error ?)
%% -> think about communicating vessels = use of accumulators
%% i.e : x/recCall() is not tail-call because of 'x/'that must be stacked.

%% test 8 ->
apply(A, Operator, B) ->
  case Operator of
    '+' -> call_sum_here;
    '/' -> call_division_here;
    _ -> no_op
  end.