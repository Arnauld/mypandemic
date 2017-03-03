%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. mars 2017 13:09
%%%-------------------------------------------------------------------
-module(city_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_not_be_infected_by_default__test() ->
  City = city:new(london),
  ?assertEqual(0, city:infection_level(City, blue)).

should_increase_infection_level_when_infected__test() ->
  City1 = city:new(london),
  City2 = city:infects(City1, blue),
  ?assertEqual(1, city:infection_level(City2, blue)).
