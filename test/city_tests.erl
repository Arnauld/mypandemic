%%%-------------------------------------------------------------------
%%% >  c("test/city_tests"), c("src/city"), eunit:test(city_tests, [verbose]).
%%%-------------------------------------------------------------------
-module(city_tests).

-include_lib("eunit/include/eunit.hrl").

%% Do not forget this result pattern when implementing city process
new_city__test() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  %
  % Whereas testing the city internal structure is not a good thing,
  % we expose it to ease initial implementation
  %
  ExpectedResult = {'Paris', ['London', 'Essen'], #{}},
  ?assertEqual(ExpectedResult, City).

%% 2
city_should_expose_its_name__test() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  CityName = city:name(City),
  ?assertEqual('Paris', CityName).

%% 3
city_should_expose_its_neighbours__test() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  Neighbours = city:neighbours(City),
  ?assertEqual(['London', 'Essen'], Neighbours).

%% 4
city_should_not_be_infected_dy_default__test() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  InfectionLevel = city:infection_level(City, blue),
  ?assertEqual(0, InfectionLevel).

%% 5
infecting_a_city_should_raise_infection_level__test() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  {infected, InfectedCity} = city:infect(City, blue),
  InfectionLevel = city:infection_level(InfectedCity, blue),
  ?assertEqual(1, InfectionLevel).

%% 6
infecting_a_city_should_raise_infection_level__multiple_diseases__test() ->
  {ok, City} = city:new('Paris', ['London', 'Essen']),
  {infected, InfectedCity1} = city:infect(City, blue),
  {infected, InfectedCity2} = city:infect(InfectedCity1, red),
  ?assertEqual(1, city:infection_level(InfectedCity2, blue)),
  ?assertEqual(1, city:infection_level(InfectedCity2, red)),
  %
  % LEt's continue to infect
  % 
  {infected, InfectedCity3} = city:infect(InfectedCity2, red),
  {infected, InfectedCity4} = city:infect(InfectedCity3, blue),
  {infected, InfectedCity5} = city:infect(InfectedCity4, blue),
  ?assertEqual(3, city:infection_level(InfectedCity5, blue)),
  ?assertEqual(2, city:infection_level(InfectedCity5, red)).


%%
%% Think about pattern matching (you case use case ... of)
%%
%% 7
city_should_raise_an_outbreak_when_infection_level_reaches_the_threshold__test() ->
  {ok, City} = city:new(london, [essen, paris]),
  {infected, InfectedCity1} = city:infect(City, red),
  {infected, InfectedCity2} = city:infect(InfectedCity1, red),
  {infected, InfectedCity3} = city:infect(InfectedCity2, red),
  Result = city:infect(InfectedCity3, red),
  ?assertEqual(outbreak, Result).


