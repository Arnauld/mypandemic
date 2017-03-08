%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. mars 2017 13:09
%%%-------------------------------------------------------------------
-module(city).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/1, infection_level/2, infects/2]).
-define(THRESHOLD, 3).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new(CityName) ->
  {CityName, #{}}.

infection_level(City, Disease) ->
  {_CityName, Levels} = City,
  maps:get(Disease, Levels, 0).


infects(City, Disease) ->
  {CityName, Levels} = City,
  Level = maps:get(Disease, Levels, 0),
  case Level of
    ?THRESHOLD ->
      outbreak;

    _ ->
      NewLevels = Levels#{Disease => Level + 1},
      {infected, {CityName, NewLevels}}
  end.
