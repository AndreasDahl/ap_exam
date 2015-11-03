-module(coordinator).
-export([init/1, wait/3, terminate/3]).


%%%=========================================================================
%%%  API
%%%=========================================================================

init([{replicas, NumReplica}, {mod, Mod}]) ->
    io:format("init coordinator : ~p ~p~n", [NumReplica, Mod]),
    {ok, Replica} = gen_fsm:start(replica, {init, self(), Mod}, []),
    {ok, wait, {wait, Replica}}.

wait(Event, Caller, {wait, Replica}) ->
    case Event of
        {read, Request} ->
            io:format("wait coordinator read~n"),
            gen_fsm:send_event(Replica, {read, Caller, Request}),
            {next_state, wait, {wait, Replica}};
        {write, Request} ->
            io:format("wait coordinator write~n"),
            gen_fsm:reply(Caller, gen_fsm:sync_send_event(Replica, {write, Request})),
            {next_state, wait, {wait, Replica}}
    end.



terminate(_Reason, _StateName, {wait, Replica}) ->
    gen_fsm:stop(Replica).
