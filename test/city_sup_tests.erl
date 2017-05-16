%%%-------------------------------------------------------------------
%%% >  c("test/city_sup_tests"), c("src/city_sup"), eunit:test(city_sup_tests, [verbose]).
%%%-------------------------------------------------------------------
-module(city_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%% HOW TO SPAWN A CHILD :
%% spawnChild(Name, Neighbours) ->
%%   {ok, Pid} = city_proc:start(Name, Neighbours),
%%   register(Name, Pid),
%%   monitor(process, Pid).
supervisor_should_start_and_register_new_city_process__tes() ->
  {ok, Pid} = city_sup:start(paris, [london, essen]),
  timer:sleep(50),
  try
    %% a process should be bound to 'paris'
    ParisPid = whereis(paris),
    ?assertEqual(true, is_pid(ParisPid)),
    ?assertEqual({paris, blue, 0}, city_proc:infection_level(paris, blue)),
    error_logger:info_msg("Test done~n")
  after
    stop_process(Pid, kill),
    stop_process(paris, kill)
  end.

%% a monitored process sends a message of the form :
%% {'DOWN', Ref, process, Pid, Reason}
%% before terminating
%% Reason = normal when it is not a failure
supervisor_should_restart_the_city_process_after_a_failure__tes() ->
  {ok, Pid} = city_sup:start(london, [paris, essen]),
  timer:sleep(50),
  try
    city_proc:infect(london, blue),
    stop_process(london, oops),

    %% gives some time for the process to be respawned by supervisor
    timer:sleep(100),
    %% a process should be bound to 'paris'
    LondonPid = whereis(london),
    ?assertEqual(true, is_pid(LondonPid)),
    Infection = city_proc:infection_level(london, blue),
    ?assertEqual({london, blue, 0}, Infection),
    error_logger:info_msg("Test 2 done~n")
  after
    stop_process(Pid, kill),
    stop_process(london, kill)
  end.

supervisor_should_stop_when_its_child_stops_gracefully__tes() ->
  {ok, Pid} = city_sup:start(essen, [london, paris]),
  timer:sleep(50),
  essen ! stop,
  timer:sleep(50),
  ?assertEqual(false, is_process_alive(Pid)).

stop_process(Proc, Reason) when is_pid(Proc) ->
  catch exit(Proc, Reason);
stop_process(undefined, _Reason) -> ok;
stop_process(Proc, Reason) ->
  Pid = whereis(Proc),
  catch unregister(Proc),
  stop_process(Pid, Reason).
