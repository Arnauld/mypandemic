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
  {ok, spawn_link(?MODULE, init, [Name, Neighbours])}.

init(Name, Neighbours) ->
  {ok, State} = city:new(Name, Neighbours),
  loop(State).

infection_level(Pid, Color) ->
  Pid ! {infection_level, Color, self()},
  receive
    Response -> Response
  end.

infect(Pid, Color) ->
  Pid ! {infect, Color, self()},
  receive
    Result -> Result
  end.

loop(State) ->
  receive
    {infection_level, Color, From} ->
      From ! {infection_level, city:name(State), Color, city:infection_level(State, Color)},
      loop(State);
    {infect, Color} ->
      infect(State, Color, no_reply);
    {infect, Color, From} ->
      infect(State, Color, From);
    stop -> ok
  end.

infect(State, Color, From) ->
  case city:infect(State, Color) of
    outbreak ->
      replyTo(From, State, Color, {outbreak, city:neighbours(State)}),
      loop(State);
    {infected, NewState} ->
      replyTo(From, NewState, Color, {infected, city:infection_level(NewState, Color)}),
      loop(NewState)
  end.

replyTo(no_reply, _City, _Color, _Message) -> noreply;
replyTo(From, City, Color, {Verb, Data}) ->
  From ! {Verb, city:name(City), Color, Data}.