-module(phone_fsm).
-export([start_link/1, stop/1, connect/1, disconnect/1, phone_action/2,
        inbound/1, accept/1, hangup/1, reject/1, busy/1,
        handle_event/4, init/1, handle_sync_event/4,
        idle/3, calling/2, handle_info/3, terminate/3, receiving/2,
        connected/2, callback_mode/0]).
-behaviour(gen_statem).
-record(phoneState, {phone, fsm}).

%% Starting phone controller
start_link(PhoneNumber) ->
    case gen_statem:start_link({global, ?MODULE}, ?MODULE, PhoneNumber, []) of
        {ok, Pid} -> {ok, Pid};
        {error, {_, Pid}} -> {ok, Pid}
    end.

stop(FsmPid) ->
    io:format('stopping ~p', [FsmPid]),
    gen_statem:stop(FsmPid).

connect(FsmPid) ->
    gen_statem:call(FsmPid, connect),
    ok.
disconnect(FsmPid) ->
    gen_statem:call(FsmPid, disconnect),
    ok.
%% Attaching phone controller to hlr
init(PhoneNumber) ->
    hlr:attach(PhoneNumber),
    {ok, idle, #phoneState{phone=empty, fsm=empty}}.

%sending different types of events/actions to controller
phone_action(FsmPid, Event) ->
    case Event of
        {outbound, PhoneNumber} ->
            gen_statem:cast(FsmPid, {event, {outbound, PhoneNumber}});
        reject ->
            gen_statem:cast(FsmPid, {event, reject});
        accept ->
            gen_statem:cast(FsmPid, {event, accept});
        hangup ->
            io:format('hangup'),
            gen_statem:cast(FsmPid, {event, hangup})
    end.


%% sending events between controllers, internal events
inbound(FsmPid) ->
    gen_statem:cast(FsmPid, {event, {inbound, self()}}). %% seding inbound message with own pid
accept(FsmPid) ->
    gen_statem:cast(FsmPid, {event, accept}).
hangup(FsmPid) ->
    gen_statem:cast(FsmPid, {event, hangup}).
reject(FsmPid) ->
    gen_statem:cast(FsmPid, {event, record}).
busy(FsmPid) ->
    gen_statem:cast(FsmPid, {event, busy}).


%%  stop has been sent
%% stop whatever we're doing and shut down!
handle_event(cast, stop, StateName, S=#phoneState{}) ->
    io:format("received stop event ~p~n~p~n", [StateName, S]),
    {stop, normal, S};
handle_event(cast, _Event, StateName, Data) ->
    io:format("received stop event other ~p ~p", [_Event, StateName]),
    {next_state, StateName, Data}.


%% This handles the syncronous events
handle_sync_event(Event, {Pid, _}, StateName, State) ->
    io:format('sync event ~p~n in state ~p~n', [Event, StateName]),
    case Event of
        connect -> 
            {reply, ok, idle, State#phoneState{phone=Pid}};
        disconnect -> 
            {reply, ok, idle, State#phoneState{phone=empty}}
    end.

%% idle state
idle({call, _From}, {event, Event}, #phoneState{phone=Phone} = State) ->
  case Event of
    %% Events that are 'invalid' in the idle state
    reject ->
         phone:reply(Phone, invalid),
         {next_state, idle, State};
    busy   -> 
         phone:reply(Phone, invalid),
         {next_state, idle, State};
    accept -> 
            phone:reply(Phone, invalid),
          {next_state, idle, State};
    hangup -> 
         phone:reply(Phone, invalid),
         {next_state, idle, State};
    {hangup, _Pid} -> 
         phone:reply(Phone, invalid),
         {next_state, idle, State};

    %% avaliable events in state idle
    {outbound, PhoneNumber} ->
        Val = hlr:lookup_id(PhoneNumber),
        case Val of
            {ok, ToPid} ->
                inbound(ToPid),
                {next_state, calling, State#phoneState{fsm=ToPid}, 10000}; 
            {error, invalid} ->  % if the number doesn exist in the register
                phone:reply(Phone, invalid),
                {next_state, idle, State}
      end;
    {inbound, Pid} ->
        Val2 = hlr:lookup_phone(Pid),
        case Val2 of
            {ok, IncomingNumber} ->
                phone:reply(Phone, {inbound, IncomingNumber}),
                {next_state, receiving, State#phoneState{fsm=Pid}};
            {error, invalid} -> % the pid of the phone doesst exist in the register
                phone:reply(Phone, invalid),
                {next_state, idle, State}
        end;

    _Other ->
      {next_state, idle, State}
  end.

%% calling state
calling({event, Event},  #phoneState{phone=Phone, fsm=Fsm} = State) ->
  case Event of
    reject ->
        phone:reply(Phone, reject),
        {next_state, idle, State};
    busy ->
        phone:reply(Phone, busy),
        {next_state, idle, State};
    {inbound, Pid} ->
        busy(Pid),
        phone:reply(Phone, busy),
        {next_state, calling, State};
    {outbound, _} ->
         phone:reply(Phone, invalid),
         {next_state, calling, State};
    {hangup, _Pid} ->
        phone:reply(Phone, hangup),
        {next_state, idle, State};
    hangup ->
        hangup(Fsm),
        {next_state, idle, State};
    accept ->
      phone:reply(Phone, accept),
      {next_state, connected, State};
    _Other ->
      {next_state, calling, State}
  end.

receiving({event, Event}, #phoneState{fsm=Fsm, phone=Phone} = State) ->
  case Event of
    hangup -> 
         phone:reply(Phone, invalid),
         {next_state, receiving, State};
    {hangup, _Pid} ->
        phone:reply(Phone, hangup),
        {next_state, idle, State};
    busy   ->
         phone:reply(Phone, invalid),
         {next_state, receiving, State};
    {inbound, Pid} ->
        busy(Pid),
        phone:reply(Phone, busy),
        {next_state, receiving, State};

    {outbound, _} ->
         phone:reply(Phone, invalid),
         {next_state, receiving, State};
    reject ->
        reject(Fsm),
        phone:reply(Phone, reject),
        {next_state, idle, State};

    accept ->
        accept(Fsm),
        phone:reply(Phone, accept),
        {next_state, connected, State};

    _Other ->
        {next_state, receiving, State}
  end.

connected({event, Event}, #phoneState{phone=PhoneNumber, fsm=Fsm} = State) ->
    case Event of
        {outbound, _ } -> 
            phone:reply(PhoneNumber, invalid),
            {next_state, connected, State};
        reject ->
            phone:reply(PhoneNumber, invalid),
            {next_state, connected, State};
        busy -> 
            phone:reply(PhoneNumber, invalid),
            {next_state, connected, State};
        {inbound, Pid} ->
            busy(Pid),
            {next_state, connect, State};
        accept ->
            {next_state, connect, State};
        hangup ->
            io:format('~n Hangup pid'),
            hangup(Fsm),
            phone:reply(PhoneNumber, hangup),
            {next_state, idle, State};
        {hangup, Pid} ->
            io:format('~n Hangup pid: ~p~n', [Pid]),
            phone:reply(PhoneNumber, hangup),
            {next_state, idle, State}
    end.

terminate(Reason, StateName, Data) ->
    io:format(' ~n terminal fsm: ~p ~p ~p~n', [Reason, StateName, Data]),
    hlr:detach().

handle_info({'EXIT', _Pid, _Reason}, StateName, StateData) ->
  {next_state, StateName, StateData}.


callback_mode()->
    state_functions.