#HSLIDE

## Intro...


#HSLIDE

## Basic Commands

#VSLIDE

```erlang
atom.
1+3.
A = 23.
LS = [1, 2, 3].
[Head|Tail] = LS.
[je_suis_une_liste_de_un_atom_et_de_deux_entier, 2, 3].
Head.
Tail.
Levels = #{blue => 0}.
LevelA = maps:get(blue, Levels).
LevelB = maps:get(gray, Levels, 0).
```

#VSLIDE

```erlang
-module(calc).
-export([add/2]).
add(A,B) ->
    A+B.
```

```erlang
-module(calc).
-export([add/2, divide/2]).
add(A,B) ->
    A+B.
    
divide(A,B) ->
    A/B.
```


```erlang
-module(calc).
-export([add/2, divide/2]).
add(A,B) ->
    A+B.
    
divide(A,0) -> infinity;
divide(A,B) ->
    A/B.
```


```erlang
-module(calc).
-export([add/2, divide/2]).
add({Ax,Ay},{Bx,By}) ->
    {Ax+Bx, Ay + By};
add(A,B) ->
    A+B.
    
divide(A,0) -> infinity;
divide(A,B) ->
    A/B.
```


```erlang
-module(calc).
-export([add/1, add/2, divide/2]).

add(Ls) ->
  add(Ls, 0).

add([], Total) -> Total;
add([L|LS], Total) -> 
  add(LS, L + Total);
add({Ax,Ay},{Bx,By}) ->
    {Ax+Bx, Ay + By};
add(A,B) ->
    A+B.
    
divide(A,0) -> infinity;
divide(A,B) ->
    A/B.
```


#HSLIDE

## City

Create a module `city` with the following functions:

* `new(CityName) -> City`
* `infects(City, Disease)`
* `infection_level(City, Disease) -> Level`

#VSLIDE

```
$ mkdir mypandemic
$ cd mypandemic
$ mkdir src && mkdir test
$ touch test/city_test.erl
$ touch src/city.erl
$ <open my ide>
```

#VSLIDE


```erlang
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
```

#VSLIDE

```erlang
-module(city).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([new/1, infection_level/2, infects/2]).

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
  NewLevels = Levels#{Disease => Level + 1},
  {CityName, NewLevels}.
```

#VSLIDE


## Handle outbreak

When infection level is already at 3, a new infect should cause an **outbreak**

```erlang
infects(City, Disease) -> 
  {infected, NewCity} 
  | outbreak
```
