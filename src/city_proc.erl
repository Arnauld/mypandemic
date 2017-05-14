%%%-------------------------------------------------------------------
%%% @author Domo-kun
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. mai 2017 21:51
%%%-------------------------------------------------------------------
-module(city_proc).
-author("Domo-kun").

%% API
-export([start_link/2, init/2, infection_level/2, infect/2]).
start_link(Name, Neighbours) ->
  {ok ,spawn(?MODULE, init, [Name, Neighbours])}.

init(Name, Neighbours) ->
  {ok, State} = city:new(Name, Neighbours),
  loop(State).

infection_level(Pid, Color) ->
  Pid ! {infection_level, Color, self()},
  receive
    {ok, Level} -> Level
  end.

infect(Pid, Color) ->
  Pid ! {infect, Color, self()},
  receive
    {ok, Result} -> Result
  end.

loop(State) ->
  receive
    {infection_level, Color, From} ->
      From ! {ok, city:infection_level(State, Color)},
      loop(State);
    {infect, Color, From} ->
      case city:infect(State, Color) of
        outbreak ->
          replyTo(From, {outbreak, city:neighbours(State)}),
          loop(State);
        {infected, NewState} ->
          replyTo(From, {infected, city:name(State)}),
          loop(NewState)
      end;
    stop -> ok
  end.

replyTo(no_reply, _Message) -> noreply;
replyTo(From, Message) ->
  From ! {ok, Message}.