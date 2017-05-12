%%%-------------------------------------------------------------------
%%% >  c("test/city_proc_tests"), c("src/city_proc"), eunit:test(city_proc_tests, [verbose]).
%%%-------------------------------------------------------------------
-module(city_proc_tests).

-include_lib("eunit/include/eunit.hrl").

city_should_start_and_responds_to_infection_level_message__test() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  Pid ! {infection_level, blue, self()},
  receive
    {ok, Level} ->
      ?assertEqual(0, Level)
  after
    500 ->
      error(timeout)
  end.


city_should_start_and_expose_its_infection_level__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  Level = city_proc:infection_level(Pid, blue),
  ?assertEqual(0, Level).


city_should_start_and_responds_to_infect_message__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  Pid ! {infect, blue, self()},
  receive
    {ok, InfectionResult} ->
      ?assertEqual({infected, paris}, InfectionResult)
  after
    500 ->
      error(timeout)
  end.


city_should_start_and_be_infected__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  InfectionResult = city_proc:infect(Pid, blue),
  ?assertEqual({infected, paris}, InfectionResult).


%% for tests purpose, we can tell the process no to reply,
%% the behaviour of the response can be coded like this :
%% replyTo(no_reply, _Response) -> ok;
%% replyTo(From, Result) ->
%%  From ! {ok, Result}.
city_should_start_and_responds_to_infect_message_until_outbreak__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  Pid ! {infect, blue, no_reply},
  Pid ! {infect, blue, no_reply},
  Pid ! {infect, blue, no_reply},
  Pid ! {infect, blue, self()},
  receive
    {ok, InfectionResult} ->
      ?assertEqual({outbreak, [london, essen]}, InfectionResult)
  after
    500 ->
      error(timeout)
  end.


city_should_start_and_should_outbreak_after_4_infections__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  city_proc:infect(Pid, blue),
  city_proc:infect(Pid, blue),
  city_proc:infect(Pid, blue),
  OutbreakResult = city_proc:infect(Pid, blue),
  ?assertEqual({outbreak, [london, essen]}, OutbreakResult).


each_city_should_start_in_its_own_process__tes() ->
  {ok, Pid1} = city_proc:start_link(paris, [london, essen]),
  {ok, Pid2} = city_proc:start_link(paris, [london, essen]),
  ?assertNotEqual(Pid1, Pid2).

city_should_start_and_stop_gracefully__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  Pid ! stop,
  timer:sleep(500), %% unnecessary ?.
  ?assertEqual(false, is_process_alive(Pid)).