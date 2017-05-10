%%%-------------------------------------------------------------------
%%% >  c("test/city_tests"), c("src/city"), eunit:test(city_tests, [verbose]).
%%%-------------------------------------------------------------------
-module(city_tests).

-include_lib("eunit/include/eunit.hrl").

new_city__test() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  %
  % Whereas testing the city internal structure is not a good thing,
  % we expose it to ease initial implementation
  %
  ExpectedResult = {'Paris', ['London', 'Essen'], #{}},
  ?assertEqual(ExpectedResult, City).

%% 2
city_should_expose_its_name__tes() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  CityName = city:name(City),
  ?assertEqual('Paris', CityName).

%% 3
city_should_expose_its_neighbours__tes() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  Neighbours = city:neighbours(City),
  ?assertEqual(['London', 'Essen'], Neighbours).

%% 4
city_should_not_be_infected_dy_default__tes() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  InfectionLevel = city:infection_level(City, blue),
  ?assertEqual(0, InfectionLevel).

%% 5
infecting_a_city_should_raise_infection_level__tes() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  {infected, InfectedCity} = city:infect(City, red),
  InfectionLevel = city:infection_level(InfectedCity, red),
  ?assertEqual(1, InfectionLevel).

%%
%% Think about pattern matching (you case use case ... of)
%%
%% 6
city_should_raise_an_outbreak_when_infection_level_reaches_the_threshold__tes() ->
  {ok, City} = city:new(london, [essen, paris]),
  {infected, InfectedCity1} = city:infect(City, red),
  {infected, InfectedCity2} = city:infect(InfectedCity1, red),
  {infected, InfectedCity3} = city:infect(InfectedCity2, red),
  Result = city:infect(InfectedCity3, red),
  ?assertEqual(outbreak, Result).


