-module(gen_replicated).
-export([start/2, stop/1, read/2, write/2]).


%%%=========================================================================
%%%  API
%%%=========================================================================

% Returns {ok, ServerRef} on success or {error, Reason} if some error occurred.
start(_NumReplica, _Mod) ->
    {error, not_yet_implemented}.

% Clients waiting for a read or write request should get the reply
% {'ABORTED', server_stopped}.
stop(_Server) ->
    undefined.

% for sending a read request to a replicated server.
% The coordinator will forward the request to one of the replica.
% The return value is {ok, Result} where Result is the result from calling the
% Mod:handle_read function. If the Mod:handle_read call raises a throw
% exception with value Val, then this function should return
% {'ABORTED', exception, Val}.
read(_Server, _Req) ->
    undefined.

% write(ServerRef, Request) for sending a write request to a replicated server.
% The coordinator will wait until there are no ongoing read nor write requests,
% and then forward the request to one of the replica.
% The return value is {ok, Result} where Result is reply from the
% Mod:handle_write function. If the write request resulted in a new state then
% all replica should be updated to the updated state.
% If the Mod:handle_write call raises a throw exception with value Val, then
% this function should return {'ABORTED', exception, Val}.
write(_Server, _Req) ->
    undefined.
