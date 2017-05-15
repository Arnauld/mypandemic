%%%-------------------------------------------------------------------
%%% >  c("test/city_proc_tests"), c("src/city_proc"), eunit:test(city_proc_tests, [verbose]).
%%%-------------------------------------------------------------------
-module(city_proc_tests).

-include_lib("eunit/include/eunit.hrl").

city_should_start_and_responds_to_infection_level_message__test() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  Pid ! {infection_level, blue, self()},
  receive
    Response ->
      ?assertEqual({infection_level, paris, blue, 0}, Response)
  after
    500 ->
      error(timeout)
  end.


city_should_start_and_expose_its_infection_level__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  Response = city_proc:infection_level(Pid, blue),
  ?assertEqual({infection_level, paris, blue, 0}, Response).


city_should_start_and_responds_to_infect_message__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  Pid ! {infect, blue, self()},
  receive
    InfectionResult ->
      ?assertEqual({infected, paris, blue, 1}, InfectionResult)
  after
    500 ->
      error(timeout)
  end.


city_should_start_and_be_infected__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  InfectionResult = city_proc:infect(Pid, blue),
  ?assertEqual({infected, paris, blue, 1}, InfectionResult).

city_should_start_and_responds_to_infect_message_until_outbreak__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  Pid ! {infect, blue}, %% async -> no reply
  Pid ! {infect, blue},
  Pid ! {infect, blue},
  Pid ! {infect, blue, self()},
  receive
    InfectionResult ->
      ?assertEqual({outbreak, paris, blue, [london, essen]}, InfectionResult)
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
  ?assertEqual({outbreak, paris, blue, [london, essen]}, OutbreakResult).


each_city_should_start_in_its_own_process__tes() ->
  {ok, Pid1} = city_proc:start_link(paris, [london, essen]),
  {ok, Pid2} = city_proc:start_link(paris, [london, essen]),
  ?assertNotEqual(Pid1, Pid2).

city_should_start_and_stop_gracefully__tes() ->
  {ok, Pid} = city_proc:start_link(paris, [london, essen]),
  Pid ! stop,
  timer:sleep(50), %% unnecessary ?.
  ?assertEqual(false, is_process_alive(Pid)).