-module(rpl).
-export([start/0, loop/1]).
start() ->
  spawn(?MODULE, loop, [0]).

loop(Count) ->
  io:format("Waiting for message~n"),
  receive
    {infect, What} ->
      NewCount = Count + 1,
      io:format("Erf... i'm infected by ~p: ~p~n", [What, NewCount]),
      loop(NewCount);
    Msg ->
      io:format("Hey: ~p~n", [Msg]),
      loop(Count)
  end.