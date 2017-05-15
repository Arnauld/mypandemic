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
-export([start_link/2, loop/2]).
start_link(Name, Neighbours) ->
  {ok, spawn_link(?MODULE, loop, [Name, Neighbours])}.

loop(Name, Neighbours) ->
  Ref = spawnChild(Name, Neighbours),
  receive
    {'DOWN', Ref, process, _Pid, normal} ->
      io:format("No more reason to live...~n"),
      terminate;
    {'DOWN', Ref, process, _Pid, _Reason} ->
      io:format("OMG City ~p Process Down !!! Restarting it...~n", [Name]),
      loop(Name, Neighbours)
  end.

spawnChild(Name, Neighbours) ->
  %% I don't want to die if my child dies due to spawn_link
  process_flag(trap_exit, true),
  {ok, Pid} = city_proc:start_link(Name, Neighbours),
  register(Name, Pid),
  monitor(process, Pid).