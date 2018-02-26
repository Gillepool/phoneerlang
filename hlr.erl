-module(hlr).
-export([start_link/0, attach/1,
         detach/0, lookup_id/1, 
         lookup_phone/1, init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
-behaviour(gen_server).
-define(DB, db).

start_link() ->
    case gen_server:start_link({global, ?MODULE}, ?MODULE, ?DB, []) of
        {ok, Pid} -> {ok, Pid};
        {error, {_, Pid}} -> {ok, Pid}
    end.


%%
%%Client functions
%%
attach(PhoneNumber) -> 
    gen_server:call({global, ?MODULE}, {event, {attach, PhoneNumber}}).
detach() -> 
    gen_server:call({global, ?MODULE}, {event, detach}).
lookup_id(PhoneNumber) ->
    gen_server:call({global, ?MODULE}, {event, {lookup_id, PhoneNumber}}).
lookup_phone(Pid) ->
    gen_server:call({global, ?MODULE}, {event, {lookup_phone, Pid}}).



init(DbModule) -> 
    State = {DbModule, DbModule:new()},
    {ok, State}.


handle_call({event, Event}, {Pid, _}, {DbModule, Db}) ->
    case Event of
        {lookup_id, PhoneNumber} ->
            Val = DbModule:read(PhoneNumber, Db),
            io:format('Value: ~p', [Val]),
            case Val of
                [] -> {reply, {error, invalid}, {DbModule, Db}};
                _ -> {reply, Val, {DbModule, Db}}
            end;
           %% io:format("looking up shizzle ~p~n", [PhoneNumber]),
           %% {reply,  DbModule:read(PhoneNumber, Db), {DbModule, Db}};
        {lookup_phone, Pid2} ->
             Val = DbModule:match(Pid2, Db),
            case Val of
                [] -> {reply, {error, invalid}, {DbModule, Db}};
                _ -> {reply, Val, {DbModule, Db}}
            end;
            %%{reply,  DbModule:match(Pid2, Db), {DbModule, Db}};
        {attach, PhoneNumber} ->
            {reply, ok, {DbModule, DbModule:write(PhoneNumber, Pid, Db)}};
        detach ->
            Val2 = DbModule:match(Pid, Db),
            io:format('match: ~p~n', [Val2]),
            case Val2 of
                [] -> {reply, ok, {DbModule, Db}};
                {_, Key} -> {reply, ok, {DbModule, DbModule:delete(Key, Db)}}
            end
    end.


handle_cast(stop, State) ->
    io:format('hello ~p', [State]).
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
terminate(_Reason, {DbModule, Db}) ->
    io:format('closing everything beacause: ~p', [_Reason]),
    DbModule:destroy(Db),
    ok.