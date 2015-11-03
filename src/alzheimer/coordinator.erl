-module(coordinator).
-behaviour(gen_server).
-export([init/1, handle_call/3, terminate/3]).


%%%=========================================================================
%%%  API
%%%=========================================================================

init([{replicas, NumReplica}, {mod, Mod}]) ->
    io:format("init coordinator : ~p ~p~n", [NumReplica, Mod]),
    Replicas = init_replicas(Mod, NumReplica),
    {ok, Replicas}.

handle_call(Event, Caller, [Replica | Replicas]) ->
    case Event of
        {read, Request} ->
            io:format("coordinator read~n"),
            gen_server:cast(Replica, {read, Caller, Request}),
            {noreply, [Replica | Replicas]};
        {write, Request} ->
            io:format("coordinator write~n"),
            Result = gen_server:call(Replica, {write, Request}),
            case Result of
                {noupdate, Reply} -> {reply, Reply, [Replica | Replicas]};
                {updated, Reply, NewState} ->
                    lists:map(fun(Rep) -> gen_server:call(Rep, {update, NewState}) end, [Replica | Replicas]),
                    {reply, Reply, [Replica | Replicas]};
                stop -> gen_server:stop(self())
            end
    end.



terminate(_Reason, _StateName, Replicas) ->
    lists:map(fun gen_server:stop/1, Replicas).

% Internals

init_replicas(_, 0) -> [];
init_replicas(Mod, NumReplica) ->
    {ok, Replica} = gen_server:start(replica, {init, Mod}, []),
    [Replica | init_replicas(Mod, NumReplica-1)].
