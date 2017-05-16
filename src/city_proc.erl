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
-export([start/2, infection_level/2, infect/2, infect_async/3]).
-export([init/2]).
start(Name, Neighbours) ->
  {ok, spawn(?MODULE, init, [Name, Neighbours])}.

init(Name, Neighbours) ->
  {ok, State} = city:new(Name, Neighbours),
  loop(State).

infection_level(Pid, Color) ->
  Pid ! {infection_level, Color, self()},
  receive
    {infection_level, CityName, Color, Level} ->
      {CityName, Color, Level}
  end.

infect(Pid, Color) ->
  infect_async(Pid, Color, self()),
  receive
    Result -> Result
  end.

infect_async(Pid, Color, ReplyTo) ->
  Pid ! {infect, Color, ReplyTo}.

loop(State) ->
  receive
    {infection_level, Color, From} ->
      From ! {infection_level, city:name(State), Color, city:infection_level(State, Color)},
      loop(State);
    {infect, Color, From} ->
      NewState = infect(State, Color, From),
      loop(NewState);
    stop -> ok
  end.

infect(State, Color, From) ->
  case city:infect(State, Color) of
    outbreak ->
      replyTo(From, {outbreak, city:name(State), Color, city:neighbours(State)}),
      State;
    {infected, NewState} ->
      replyTo(From, {infected, city:name(NewState), Color, city:infection_level(NewState, Color)}),
      NewState
  end.

replyTo(no_reply, _Message) -> noreply;
replyTo(From, Message) ->
  From ! Message.