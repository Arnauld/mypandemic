%%%-------------------------------------------------------------------
%%% @author Domo-kun
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. mai 2017 22:10
%%%-------------------------------------------------------------------
-module(city_sup).
-author("Domo-kun").

%% API
-export([start_link/0, init/0, new_child/2]).
start_link() ->
  {ok, spawn(?MODULE, init, [])}.

init() ->
  register(?MODULE, self()),
  loop(#{}).

new_child(Name, Neighbours) ->
  ?MODULE ! {spawn_child, Name, Neighbours}.

loop(State) ->
  receive
    {spawn_child, Name, Neighbours} ->
      spawnChild(Name, Neighbours, State);
    {'DOWN', Ref, process, _Pid, normal} ->
      NewState = maps:remove(Ref, State),
      case maps:size(NewState) of
        0 -> terminate;
        _ -> loop(State)
      end;
    {'DOWN', Ref, process, _Pid, _Reason} ->
      {Name, Neighbours} = maps:get(Ref, State),
      spawnChild(Name, Neighbours, State)
  end.

spawnChild(Name, Neighbours, State) ->
  {ok, Pid} = city_proc:start_link(Name, Neighbours),
  register(Name, Pid),
  Ref = monitor(process, Pid),
  loop(State#{Ref => {Name, Neighbours}}).