-module(gen_replicated).
-export([start/2, stop/1, read/2, write/2, test/0]).


%%%=========================================================================
%%%  API
%%%=========================================================================

% Returns {ok, ServerRef} on success or {error, Reason} if some error occurred.
start(NumReplica, Mod) ->
    gen_server:start({local, coordinator}, coordinator, [{replicas, NumReplica},
        {mod, Mod}], []).

% Clients waiting for a read or write request should get the reply
% {'ABORTED', server_stopped}.
stop(Server) ->
    gen_server:stop(Server).

% for sending a read request to a replicated server.
% The coordinator will forward the request to one of the replica.
% The return value is {ok, Result} where Result is the result from calling the
% Mod:handle_read function. If the Mod:handle_read call raises a throw
% exception with value Val, then this function should return
% {'ABORTED', exception, Val}.
read(Server, Req) ->
    gen_server:call(Server, {read, Req}).

% write(ServerRef, Request) for sending a write request to a replicated server.
% The coordinator will wait until there are no ongoing read nor write requests,
% and then forward the request to one of the replica.
% The return value is {ok, Result} where Result is reply from the
% Mod:handle_write function. If the write request resulted in a new state then
% all replica should be updated to the updated state.
% If the Mod:handle_write call raises a throw exception with value Val, then
% this function should return {'ABORTED', exception, Val}.
write(Server, Req) ->
    gen_server:call(Server, {write, Req}).


test() ->
    {ok, Pid} = start(3, simple_mod),
    write(Pid, one),
    write(Pid, two),
    read(Pid, 1),
    write(Pid, three),
    read(Pid, 3),
    stop(Pid).
