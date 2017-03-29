%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. mars 2017 19:12
%%%-------------------------------------------------------------------
-module(city_sup).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start(CityName, Links) ->
  spawn(?MODULE, supervise, [CityName, Links]).

supervize(CityName, Links) ->
  Pid = city_proc:start(CityName, Links),
  Ref = monitor(process, Pid),
  register(CityName, Pid),
  receive
    {'DOWN', Ref, process, Pid, Reason} ->
      io:format("City ~p down... ~p~n",  [CityName, Reason]),
      supervize(CityName, Links)
  end.