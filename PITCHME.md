#HSLIDE

## Intro...


#HSLIDE

## Erlang Basics - Shell & Numbers

```bash
$ erl
Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.1  (abort with ^G)
1> 1+3.
4
2> 123456789 * 123456789 * 123456789 * 123456789.
232305722798259244150093798251441
3> 5/2. 
2.5
4> is_integer(2.5).
false
5> is_float(232305722798259244150093798251441.0).
true
```

#VSLIDE

## Erlang Basics - Variable

```erlang
6> A = 23.
23
7> 23 = A.
23
8> 23 = B.
* 1: variable 'B' is unbound
9> C.
* 1: variable 'C' is unbound
10> A.
23
11> A = A + 1.
** exception error: no match of right hand side value 24
12> f().
ok
13> A.
* 4: variable 'A' is unbound
```

#VSLIDE

## Erlang Basics - Atom

```erlang
14> threshold = 12.
** exception error: no match of right hand side value 12
15> true.
true
16> false.
false
17> i_am_an_atom.
i_am_an_atom
```

#VSLIDE

## Erlang Basics - Tuple

```erlang
18> {devoxx, paris, france}.
{devoxx, paris, france}
19> {erlang, 5}.
{erlang, 5}
20> HandsOn = {erlang, 200}.
{erlang, 5}
21> {What, Room} = HandsOn.
erlang,5}
22> What.
erlang
23> Room.
200
```

#VSLIDE

## Erlang Basics - List

```erlang
24> [paris, essen, madrid].
[paris, essen, madrid]
25> LS = [paris, essen, madrid].
26> [Head | Tail] = LS. 
27> Head.
paris
28> Tail.
[essen, madrid]
29> [First, Second | Remaining] = LS.
30> First.
paris
31> Second.
essen
32> [madrid | Remaining2] = Remaining.
33> Remaining2.
[]
```


## Erlang Types
 
* Numbers 
 
```erlang
1> 1 + 4.
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

### But before...

```erlang
case Value of
  0 -> zero;
  1 -> one;
  2 -> two;
  ping -> pong;
  _ -> i_don_t_know
end
```

#VSLIDE

## Handle outbreak

When infection level is already at 3, a new infect should cause an **outbreak**

```erlang
infects(City, Disease) -> 
  {infected, NewCity} 
  | outbreak
```

#VSLIDE

```erlang
-module(city_tests).

-include_lib("eunit/include/eunit.hrl").

should_not_be_infected_by_default__test() ->
  City = city:new(london),
  ?assertEqual(0, city:infection_level(City, blue)).

should_increase_infection_level_when_infected__test() ->
  City1 = city:new(london),
  {infected, City2} = city:infects(City1, blue),
  ?assertEqual(1, city:infection_level(City2, blue)).

should_outbreak_when_infection_level_reaches_the_threshold__test() ->
  City1 = city:new(london),
  {infected, City2} = city:infects(City1, blue),
  {infected, City3} = city:infects(City2, blue),
  {infected, City4} = city:infects(City3, blue),
  Result = city:infects(City4, blue),
  ?assertEqual(outbreak, Result).
```

```erlang
-module(city).

-export([new/1, infection_level/2, infects/2]).
-define(THRESHOLD, 3).

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
```