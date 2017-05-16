%%%-------------------------------------------------------------------
%%% >  c("test/city_proc_tests"), c("src/city_proc"), eunit:test(city_proc_tests, [verbose]).
%%%-------------------------------------------------------------------
-module(city_proc_tests).

-include_lib("eunit/include/eunit.hrl").

city_should_start_and_responds_to_infection_level_message__test() ->
  {ok, Pid} = city_proc:start(paris, [london, essen]),
  Pid ! {infection_level, blue, self()},
  receive
    Response ->
      ?assertEqual({infection_level, paris, blue, 0}, Response)
  after
    500 ->
      error(timeout)
  end.


city_should_start_and_expose_its_infection_level__test() ->
  {ok, Pid} = city_proc:start(paris, [london, essen]),
  Response = city_proc:infection_level(Pid, blue),
  ?assertEqual({paris, blue, 0}, Response).


city_should_start_and_responds_to_infect_message__test() ->
  {ok, Pid} = city_proc:start(paris, [london, essen]),
  Pid ! {infect, blue, self()},
  receive
    InfectionResult ->
      ?assertEqual({infected, paris, blue, 1}, InfectionResult)
  after
    500 ->
      error(timeout)
  end.

city_should_start_and_be_infected__test() ->
  {ok, Pid} = city_proc:start(paris, [london, essen]),
  InfectionResult = city_proc:infect(Pid, blue),
  ?assertEqual({infected, paris, blue, 1}, InfectionResult).

%%
%% replyTo(no_reply, _City, _Color, _Message) -> noreply;
%% replyTo(From, City, Color, {Verb, Data}) ->
%%   From ! {Verb, city:name(City), Color, Data}.
city_should_start_and_increment_infection_to_infect_async_message__test() ->
  {ok, Pid} = city_proc:start(paris, [london, essen]),
  Pid ! {infect, blue}, %% async -> no reply
  Response = city_proc:infection_level(Pid, blue),
  ?assertEqual({paris, blue, 1}, Response).

city_should_start_and_be_infected_async__test() ->
  {ok, Pid} = city_proc:start(paris, [london, essen]),
  {infect, blue} = city_proc:infect_async(Pid, blue),
  Response = city_proc:infection_level(Pid, blue),
  ?assertEqual({paris, blue, 1}, Response).

city_should_start_and_responds_to_infect_message_until_outbreak__test() ->
  {ok, Pid} = city_proc:start(paris, [london, essen]),
  Pid ! {infect, blue},
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


city_should_start_and_should_outbreak_after_4_infections__test() ->
  {ok, Pid} = city_proc:start(paris, [london, essen]),
  city_proc:infect(Pid, blue),
  city_proc:infect(Pid, blue),
  city_proc:infect(Pid, blue),
  OutbreakResult = city_proc:infect(Pid, blue),
  ?assertEqual({outbreak, paris, blue, [london, essen]}, OutbreakResult).


each_city_should_start_in_its_own_process__test() ->
  {ok, Pid1} = city_proc:start(paris, [london, essen]),
  {ok, Pid2} = city_proc:start(paris, [london, essen]),
  ?assertNotEqual(Pid1, Pid2).

city_should_start_and_stop_gracefully__test() ->
  {ok, Pid} = city_proc:start(paris, [london, essen]),
  Pid ! stop,
  timer:sleep(50), %% unnecessary ?.
  ?assertEqual(false, is_process_alive(Pid)).