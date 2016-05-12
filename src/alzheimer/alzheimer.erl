-module(alzheimer).
-export([start/0, upsert/3, query/2, test/0]).


%%%=========================================================================
%%%  API
%%%=========================================================================

% Returns {ok, Aid}
start() ->
    gen_replicated:start(5, alzheimer_mod).

% Calls P({Id, Data}) for each row in the database,
% where Data is the row data for Id.
query(Aid, Pred) ->
    gen_replicated:read(Aid, Pred).

% for inserting or updating the row with identifier Id.
upsert(Aid, Id, F) ->
    gen_replicated:write(Aid, {Id, F}).

test() ->
    {ok, Aid} = start(),
    upsert(Aid, 1, fun insert_a/1),
    upsert(Aid, 2, fun insert_a/1),
    query(Aid, fun even_key/1).

% Internals for testing

even_key({Key, _Value}) ->
    (Key rem 2) == 0.

insert_a({new, _Id}) ->
    {modify, a};
insert_a({existing, {_Id, Data}}) ->
    case Data == a of
        true -> ignore;
        false -> {modify, a}
    end.
