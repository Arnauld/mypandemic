%%%-------------------------------------------------------------------
%%% >  c("test/city_sup_tests"), c("src/city_sup"), eunit:test(city_sup_tests, [verbose]).
%%%-------------------------------------------------------------------
-module(city_sup_tests).

-include_lib("eunit/include/eunit.hrl").

supervisor_should_start_a_process__test() ->
  {ok, Pid} = city_sup:start_link(paris, [london, essen]),
  ?assertEqual(true, is_pid(Pid)).

supervisor_should_register_the_city_process__tes() ->
  city_sup:start_link(paris, [london, essen]),
  ?assertEqual(true, is_pid(whereis(paris))).

supervisor_should_restart_the_city_process_after_a_failure__tes() ->
  city_sup:start_link(paris, [london, essen]),
  ParisPid = whereis(paris),
  paris ! {infection_level, blue, make_it_fail},
  timer:sleep(500), %% unnecessary ?
  RestartedParisPid = whereis(paris),
  ?assertNotEqual(ParisPid, RestartedParisPid).