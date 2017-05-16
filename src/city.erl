%%%-------------------------------------------------------------------
%%% @author Domo-kun
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. mai 2017 21:46
%%%-------------------------------------------------------------------
-module(city).
-author("Domo-kun").

%% API
-export([new/2, name/1, infection_level/2, neighbours/1, infect/2]).
new(Name, Neighbours) ->
  {ok, {Name, Neighbours, #{}}}.

name({Name, _Neighbours, _Infections}) ->
  Name.

neighbours({_Name, Neighbours, _Infections}) ->
  Neighbours.

infection_level({_Name, _Neighbours, Infections}, Color) ->
  maps:get(Color, Infections, 0).

infect(City = {Name, Neighbours, Infections}, Color) ->
  Level = infection_level(City, Color),
  case Level of
    3 -> outbreak;
    _ -> {infected, {Name, Neighbours, Infections#{Color => Level + 1}}}
  end.