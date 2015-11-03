-module(replica).
-export([init/1, wait/2, wait/3, reading/2, writing/2, terminate/3]).


%%%=========================================================================
%%%  API
%%%=========================================================================

init({init, Coordinator, Mod}) ->
    io:format("init replica~n"),
    {ok, State} = Mod:init(),
    {ok, wait, {wait, Coordinator, Mod, State}}.

wait({read, Caller, Request}, {wait, Coordinator, Mod, State}) ->
    io:format("replica calling wait read ~n"),
    {next_state, reading, {read, Request, Caller, Coordinator, Mod, State}, 0}.

wait({write, Request}, Caller, {wait, Coordinator, Mod, State}) ->
    io:format("replica calling wait write ~n"),
    {next_state, writing, {write, Request, Caller, Coordinator, Mod, State}, 0}.


reading(timeout, {read, Request, Caller, Coordinator, Mod, State}) ->
    io:format("replica calling reading~n"),
    {reply, Result} = Mod:handle_read(Request, State),
    gen_fsm:reply(Caller, {ok, Result}),
    {next_state, wait, {wait, Coordinator, Mod, State}}.

writing(timeout, {write, Request, Caller, Coordinator, Mod, State}) ->
    io:format("replica calling writing~n"),
    {updated, Result, NewState} = Mod:handle_write(Request, State),
    gen_fsm:reply(Caller, {ok, Result}),
    {next_state, wait, {wait, Coordinator, Mod, NewState}}.


terminate(Reason, StateName, StateData) ->
    io:format("replica terminating due to ~p with state ~p and data ~p~n",
            [Reason, StateName, StateData]).
