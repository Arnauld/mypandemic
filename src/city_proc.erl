%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. mars 2017 20:11
%%%-------------------------------------------------------------------
-module(city_proc).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/2, infects/2, infects/3]).
-export([loop/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start(CityName, Links) ->
  spawn(?MODULE, loop, [city:new(CityName), Links]).

infects(_City, _Disease) ->
  erlang:error(not_implemented).

infects(_City, _Disease, _ReplyTo) ->
  erlang:error(not_implemented).

loop(City, Links) ->
  receive
    {infection_level, Disease, ReplyTo} ->
      Level = city:infection_level(City, Disease),
      ReplyTo ! {infection_level, city:name_of(City), Disease, Level},
      loop(City, Links)
  end.

