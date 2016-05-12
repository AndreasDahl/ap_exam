-module(coordinator).
-behaviour(gen_server).
-export([init/1, handle_call/3, terminate/2]).


%%%=========================================================================
%%%  API
%%%=========================================================================

init([{replicas, NumReplica}, {mod, Mod}]) ->
    % io:format("init coordinator : ~p ~p~n", [NumReplica, Mod]),
    Replicas = queue:from_list(init_replicas(Mod, NumReplica)),
    {ok, Replicas}.

handle_call(Event, Caller, Replicas) ->
    case Event of
        {read, Request} ->
            % io:format("coordinator read~n"),
            {{value, Replica}, Replicas2} = queue:out(Replicas), % Cycle the replica queue
            gen_server:cast(Replica, {read, Caller, Request}),
            {noreply, queue:in(Replica, Replicas2)};
        {write, Request} ->
            % io:format("coordinator write~n"),
            lists:map(fun(Rep) -> gen_server:call(Rep, ping) end, queue:to_list(Replicas)),
            Result = gen_server:call(queue:get(Replicas), {write, Request}),
            case Result of
                {noupdate, Reply} -> {reply, Reply, Replicas};
                {updated, Reply, NewState} ->
                    lists:map(fun(Rep) -> gen_server:call(
                        Rep, {update, NewState}) end, queue:to_list(Replicas)),
                    {reply, Reply, Replicas};
                stop -> gen_server:stop(self())
            end
    end.

terminate(_Reason, Replicas) ->
    % io:format("Coordinator ~p terminating due to ~p with state ~p~n",
            % [self(), Reason, Replicas]),
    lists:map(fun gen_server:stop/1, queue:to_list(Replicas)).

% Internals

init_replicas(_, 0) -> [];
init_replicas(Mod, NumReplica) ->
    {ok, Replica} = gen_server:start(replica, {init, Mod}, []),
    [Replica | init_replicas(Mod, NumReplica-1)].
