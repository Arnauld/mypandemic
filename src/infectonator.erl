%%%-------------------------------------------------------------------
%%% @author Domo-kun
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. mai 2017 12:38
%%%-------------------------------------------------------------------
-module(infectonator).
-author("Domo-kun").

%% API
-export([infect/2, init/2]).
infect(City, Color) ->
  spawn(?MODULE, init, [City, Color]).

init(City, Color) ->
  case city_proc:infect(City, Color) of
    {outbreak, City, Color, Neighbours} ->
      io:format("starting outbreak process of ~p~n", [City]),
      loop([City], Neighbours, Color);
    _ -> infection_end
  end.

loop(AlreadyOutbreaked, [], Color) ->
  %% get infection result and new list of cities to infect
  {OutbreakedCities, FilteredToInfect} = accumulateOutbreaks(AlreadyOutbreaked, []),
  case FilteredToInfect of
    [] -> end_of_outbreak;
    _ -> loop(OutbreakedCities, FilteredToInfect, Color)
  end;
loop(AlreadyOutbreaked, [H | T], Color) ->
  %% infects each city if it exists
  case lists:member(H, AlreadyOutbreaked) of
    true -> ignore_outbreak;
    _ -> case whereis(H) of
           undefined ->
             io:format("no city for ~p~n", [H]);
           _ ->
             city_proc:infect_async(H, Color, self())
         end
  end,
  loop(AlreadyOutbreaked, T, Color).


accumulateOutbreaks(OutbreakedCities, ToInfect) ->
  receive
    {outbreak, City, _Color, Neighbours} ->
      io:format("accumulating outbreak of ~p (~p)~n", [City, Neighbours]),
      accumulateOutbreaks([City | OutbreakedCities], Neighbours++ ToInfect);
    %% ignore non outbreak
    _ -> accumulateOutbreaks(OutbreakedCities, ToInfect)
  after 100 ->
    CitiesToInfect = removeOutbreaksFromCitiesToInfect(OutbreakedCities, ToInfect),
    io:format("new cities to infect: ~p (OutbreakedCities : ~p)~n", [CitiesToInfect, OutbreakedCities]),
    {OutbreakedCities, CitiesToInfect}
  end.

removeOutbreaksFromCitiesToInfect([], ToInfect) -> ToInfect;
removeOutbreaksFromCitiesToInfect([H | T], ToInfect) ->
  removeOutbreaksFromCitiesToInfect(T, lists:delete(H, ToInfect)).
