-module(gen_replicated).
-export([start/2, stop/1, read/2, write/2]).


%%%=========================================================================
%%%  API
%%%=========================================================================

start(_NumReplica, _Mod) ->
    undefined.

stop(_Server) ->
    undefined.

read(_Server, _Req) ->
    undefined.

write(_Server, _Req) ->
    undefined.
