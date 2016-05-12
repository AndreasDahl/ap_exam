-module(alzheimer_mod).
-export([init/0, handle_read/2, handle_write/2]).


% should return {ok, State :: term()}.
init() ->
    {ok, maps:new()}.



% Should return one of the following values
% - {noupdate, Reply :: term()} if the operation does not result in a updated state,
% – {updated, Reply :: term(), NewState :: term()}iftheoperationresults in the
%   updated state NewState,
% – stop if the server should be stopped.
handle_write({Id, F}, State) ->
    Action = case maps:get(Id, State, nothing) of
        nothing ->
            F({new, Id});
        Value ->
            F({existing, {Id, Value}})
    end,
    case Action of
        {modify, NewData} ->
            {updated, {modify, NewData}, maps:put(Id, NewData, State)};
        ignore ->
            {noupdate, ignore, State}
    end.


% Should return {reply, Reply :: term()}
% or return stop if the server should be stopped.
handle_read(P, State) ->
    {reply, maps:to_list(maps:filter(fun (K, V) -> P({K, V}) end, State))}.
