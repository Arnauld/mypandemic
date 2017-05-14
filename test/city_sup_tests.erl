%%%-------------------------------------------------------------------
%%% >  c("test/city_sup_tests"), c("src/city_sup"), eunit:test(city_sup_tests, [verbose]).
%%%-------------------------------------------------------------------
-module(city_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%% The idea here is to have one supervisor for all cities.
%%
%% Its responsibility is to spawn a city, monitor it,
%% and restart it when the city fails (loosing the city state).
%%
%% If a city terminates normally,
%% remove it from the monitored processes.
%%
%% If the last city terminates normally,
%% the supervisor has no more reason to live :)
%%
%% supervisor must keep track of monitored cities references,
%% so it can recreate them (Using Name and Neighbours data)

%% register(atom_name, a_pid),
supervisor_should_start_and_register_a_process__test() ->
  unregister_city_sup(city_sup),
  {ok, Pid} = city_sup:start_link(),
  timer:sleep(50),
  ?assertEqual(Pid, whereis(city_sup)).

%% monitor(process, Pid) to monitor a process
%% returns an identifier called Reference
supervisor_should_register_the_new_city_process__tes() ->
  supervise_paris_city(),
  ?assertEqual(true, is_pid(whereis(paris))).

supervise_paris_city() ->
  unregister_city_sup(city_sup),
  unregister_city_sup(paris),
  city_sup:start_link(),
  timer:sleep(50),
  city_sup:new_child(paris, [london, essen]),
  timer:sleep(50).

%% a monitored process sends a message of the form :
%% {'DOWN', Reference, process, Pid, Reason}
%% before terminating
%% Reference = Identifier returned when monitor was called
%% Reason = normal when it is not a failure
supervisor_should_restart_the_city_process_after_a_failure__tes() ->
  supervise_paris_city(),
  ParisPid = whereis(paris),
  paris ! {infection_level, blue, make_it_fail},
  timer:sleep(50),
  RestartedParisPid = whereis(paris),
  ?assertNotEqual(ParisPid, RestartedParisPid).

supervisor_should_stop_when_its_last_child_stops_gracefully__tes() ->
  supervise_paris_city(),
  paris ! stop,
  timer:sleep(50),
  ?assertEqual(undefined, whereis(city_sup)).

supervisor_should_still_live_when_one_of_its_children_stops_gracefully__tes() ->
  supervise_paris_city(),
  unregister_city_sup(london),
  city_sup:new_child(london, [paris, essen]),
  timer:sleep(50),
  paris ! stop,
  timer:sleep(50),
  ?assertEqual(true, is_process_alive(whereis(city_sup))).

unregister_city_sup(ProcName) ->
  case whereis(ProcName) of
    undefined -> no_op;
    _ -> case is_process_alive(whereis(ProcName)) of
           true ->
             case ProcName of
               city_sup -> exit(whereis(city_sup), kill);
               _ -> no_op
             end,
             unregister(ProcName);
           _ ->
             no_op
         end
  end.