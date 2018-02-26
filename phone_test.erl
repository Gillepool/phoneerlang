%%%-------------------------------------------------------------------
%%% @author Joel Denke, Mats Maatson
%%% @copyright (C) 2014, KTH
%%% @doc
%%%    Phone test simulator
%%% @end
%%% Created : 10. feb 2014 07:30
%%%-------------------------------------------------------------------
-module(phone_test).

%% API
-export([phone_fsm_all/0, phone_fsm_task/0, phone_simulator/0, phone_fsm_sup/0, bsc_sup/0, phone_fsm_call_myself/0,
all/0, all_tasks/0, hlr/0]).

all() ->
  all_tasks(),
  io:format("Roberts simulator\n"),
  phone_simulator(),
  ok.

all_tasks() ->
  io:format("Task 3.1 hlr\n"),
  hlr(),
  io:format("Our own to test phone fsm all functions\n"),
  phone_fsm_all(),
  io:format("Task 3.2 test\n"),
  phone_fsm_task(),
  io:format("Task 3.3 phone_fsm_sup\n"),
  phone_fsm_sup(),
  io:format("Task 3.3 bsc_sup\n"),
  bsc_sup(),
  ok.

hlr() ->
  {ok, _HLR} = hlr:start_link(),
  ok = start_links(phone_fsm, ["123"]),% Need to call hlr:attach(PhoneNumber) from a process
  {ok, FsmPid} = hlr:lookup_id("123"),
  io:format('yooyoy: ~p~n', [FsmPid]),
  {ok, _PhoneNumber} = hlr:lookup_phone(FsmPid),
  io:format('yooyoy number 2: ~p~n', [_PhoneNumber]),
  ok = phone_fsm:stop(FsmPid), % Need to call hlr:detach() from a process
  timer:sleep(1000),
  {error, invalid} = hlr:lookup_id("123"),
  %hlr:stop(),
  ok.

bsc_sup() ->
  io:format("Start Basestation Supervisor\n", []),
  {ok, Sup} = bsc_sup:start_link(),

  io:format("Add Phone FSM controllers\n", []),
  ok = bsc_mod_controllers(bsc_sup, add, Sup, ["123", "124", "125"]),

  Pid = whereis(phone_fsm_sup),

  io:format("Current controllers ~p \n", [supervisor:which_children(Pid)]),
  timer:sleep(1000),

  ok = bsc_mod_controllers(bsc_sup, remove, Sup, ["123", "124", "125"]),
  io:format("Current controllers ~p \n", [supervisor:which_children(Pid)]),
  ok.

phone_fsm_sup() ->
  io:format("Start HLR link\n", []), % Make sure hlr is started. If already started, that process will be returned
  {ok, _} = hlr:start_link(),
  io:format("Start Phone FSM Supervisor\n", []),
  {ok, Sup} = phone_fsm_sup:start_link(),

  io:format("Add Phone FSM controllers\n", []),
  ok = mod_controllers(phone_fsm_sup, add, Sup, ["123", "124", "125"]),

  io:format("Current controllers ~p \n", [supervisor:which_children(Sup)]),
  timer:sleep(1000),

  ok = mod_controllers(phone_fsm_sup, remove, Sup, ["123", "124", "125"]),

  io:format("Stop Phone FSM Supervisor\n", []),
  ok = phone_fsm_sup:stop(Sup),

  timer:sleep(2000),
  ok = lookup_phones(["123", "124", "125"]),

  %hlr:stop(),
  ok.

phone_simulator() ->
  io:format("Start HLR link\n", []),
  {ok, _HLR} = hlr:start_link(),
  io:format("Start BSC Simulator link\n", []),
  {ok,Sim} = bsc_sim:start_link(10000),
  io:format("Start BSC Simulator with 1 second between each delay\n", []),
  ok = bsc_sim:start_run(Sim, 10),
  timer:sleep(5000),
  io:format("Got status: ~p \n", [bsc_sim:get_status(Sim)]),
  timer:sleep(1000),
  io:format("Got status: ~p \n", [bsc_sim:get_status(Sim)]),
  io:format("Stop BSC Simulator\n", []),
  timer:sleep(1000),
  ok = bsc_sim:stop_run(Sim),
  timer:sleep(5000),
  io:format("Got status: ~p \n", [bsc_sim:get_status(Sim)]),
  ok.

phone_fsm_call_myself() ->
  {ok, _} = hlr:start_link(),
  ok = start_links(phone_fsm, ["123"]),
  {ok, P123} = phone:start_link("123"),
  ok = run_phone_actions([
    {P123, {call, "123"}}
  ]),
  ok = phone:stop(P123),
  ok.

phone_fsm_all() ->
  {ok, _} = hlr:start_link(),

  ok = start_links(phone_fsm, ["123", "124", "125"]),

  {ok, P123} = phone:start_link("123"),
  {ok, P124} = phone:start_link("124"),
  {ok, P125} = phone:start_link("125"),

  ok = run_phone_actions([
    {P123, {call, "124"}},
    {P124, reject},
    {P125, {call, "123"}},
    {P123, accept},
    {P123, hangup},
    {P124, {call, "125"}},
    {P125, accept},
    {P124, hangup}
  ]),

  ok = phone:stop(P123),
  ok = phone:stop(P124),
  ok = phone:stop(P125),

  ok.

phone_fsm_task() ->
  {ok, _} = hlr:start_link(),

  ok = start_links(phone_fsm, ["123", "124", "125"]),

  {ok, P123} = phone:start_link("123"),
  {ok, P124} = phone:start_link("124"),
  {ok, P125} = phone:start_link("125"),

  ok = run_phone_actions([
    {P123, {call, "124"}},
    {P124, accept},
    {P125, {call, "123"}},
    {P125, {call, "124"}}
  ]),

  ok = phone:stop(P123),
  ok = phone:stop(P124),
  ok = phone:stop(P125),

  ok.

lookup_phones(Numbers) ->
  lists:foldl(
    fun (K, _D) ->
      case hlr:lookup_id(K) of
        {ok, Pid} -> io:format("Number ~s was found with Pid ~p in HLR ~n", [K, Pid]);
        {error, invalid} -> io:format("Number ~s was not found in HLR ~n", [K])
      end,
      ok
    end,
    ok, Numbers).

bsc_mod_controllers(Mod, Mode, SupPid,  Numbers) ->
  lists:foldl(
    fun (K, _D) ->
      case Mode of
        add     ->
          {ok, Pid} = Mod:add_controller(K),
          io:format("Module ~s: Added and started process ~p with number ~s ~n", [Mod, Pid, K]);
        remove  ->
          {ok, Pid} = Mod:remove_controller(K),
          io:format("Supervisor ~p: Removed phone ~s from fsm supervisor ~p ~n", [SupPid, K, Pid])
      end,
      ok
    end,
    ok, Numbers).

mod_controllers(Mod, Mode, SupPid,  Numbers) ->
  lists:foldl(
    fun (K, _D) ->
      case Mode of
        add     ->
          {ok, Pid} = Mod:add_controller(SupPid, K),
          io:format("Module ~s: Added and started process ~p with number ~s ~n", [Mod, Pid, K]);
        remove  ->
          ok = Mod:remove_controller(SupPid, K),
          io:format("Supervisor ~p: Removed phone ~s ~n", [SupPid, K])
      end,
      ok
    end,
    ok, Numbers).

start_links(Mod, Numbers) ->
  lists:foldl(
    fun (K, _D) ->
      {ok, Pid} = Mod:start_link(K),
      io:format("Module ~s: Started process ~p with number ~s ~n", [Mod, Pid, K]),
      ok
    end,
  ok, Numbers).

run_phone_actions(Actions) ->
  lists:foldl(
    fun ({Pid, Action}, ok) ->
      io:format("Running phone action: ~p from pid ~p ~n", [Action, Pid]),
      phone:action(Pid, Action),
      case Action of
        {call, _} -> timer:sleep(3000);
        _Other     -> timer:sleep(2000)
      end,
      ok
       % Wait between actions
    end,
    ok, Actions).