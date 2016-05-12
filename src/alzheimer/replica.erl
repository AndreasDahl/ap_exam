-module(replica).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).


%%%=========================================================================
%%%  API
%%%=========================================================================

init({init, Mod}) ->
    % io:format("init replica ~p~n", [self()]),
    {ok, State} = Mod:init(),
    {ok, {Mod, State}}.

handle_cast({read, Caller, Request}, {Mod, State}) ->
    % io:format("replica ~p reading~n", [self()]),
    {reply, Result} = Mod:handle_read(Request, State),
    gen_server:reply(Caller, {ok, Result}),
    {noreply, {Mod, State}}.


handle_call(Event, _Caller, {Mod, State}) ->
    case Event of
        {write, Request} ->
            % io:format("replica ~p calling writing~n", [self()]),
            {reply, Mod:handle_write(Request, State), {Mod, State}};
        {update, NewState} ->
            % io:format("replica ~p updating state~n", [self()]),
            {reply, ok, {Mod, NewState}};
        ping ->
            % io:format("replica ~p is ready~n", [self()]),
            {reply, ok, {Mod, State}}
    end.

terminate(_Reason, _State) ->
    % io:format("replica ~p terminating due to ~p with state ~p~n",
    %         [self(), Reason, State]),
    ok.
