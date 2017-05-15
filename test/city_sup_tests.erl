%%%-------------------------------------------------------------------
%%% >  c("test/city_sup_tests"), c("src/city_sup"), eunit:test(city_sup_tests, [verbose]).
%%%-------------------------------------------------------------------
-module(city_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%% HOW TO SPAWN A CHILD :
%% spawnChild(Name, Neighbours) ->
%%   process_flag(trap_exit, true),
%%   {ok, Pid} = city_proc:start_link(Name, Neighbours),
%%   register(Name, Pid),
%%   monitor(process, Pid).
supervisor_should_start_and_register_new_city_process__test() ->
  {ok, Pid} = city_sup:start_link(paris, [london, essen]),
  timer:sleep(50),
  try
    ?assertEqual({paris, blue, 0}, city_proc:infection_level(paris, blue))
  after
    io:format("0...~n"),
    kill_city_sup(Pid, paris)
  end.


%% a monitored process sends a message of the form :
%% {'DOWN', Ref, process, Pid, Reason}
%% before terminating
%% Reason = normal when it is not a failure
supervisor_should_restart_the_city_process_after_a_failure__test() ->
  {ok, Pid} = city_sup:start_link(london, [paris, essen]),
  timer:sleep(50),
  try
    ?assertEqual({infected, london, blue, 1}, city_proc:infect(london, blue))
  after
    kill_city_sup(Pid, london)
  end.

%% kill -> infections data should be lost
%timer:sleep(50),
%kill_city(london),
%timer:sleep(50),
%%  ?assertEqual({london, blue, 1}, city_proc:infection_level(london, blue)).

supervisor_should_stop_when_its_child_stops_gracefully__test() ->
  {ok, Pid} = city_sup:start_link(essen, [london, paris]),
  timer:sleep(50),
  essen ! stop,
  timer:sleep(50),
  ?assertEqual(false, is_process_alive(Pid)).

kill_city_sup(Pid, City) ->
  try
    io:format("1...~n"),
    exit(Pid, kill),
    io:format("2...~n"),
    try exit(whereis(City), kill)
    catch _ ->
      ignore end,
    io:format("3...~n")
  catch _ ->
    ignore
  end.
