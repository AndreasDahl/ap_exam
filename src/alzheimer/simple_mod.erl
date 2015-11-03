



% should return {ok, State :: term()}.
init()

% Should return {reply, Reply :: term()}
% or return stop if the server should be stopped.
handle_read(Request :: term(), State :: term()) ->
    undefined.

% Should return one of the following values
% - {noupdate, Reply :: term()} if the operation does not result in a updated state,
% – {updated, Reply :: term(), NewState :: term()}iftheoperationresults in the updated state NewState,
% – stop if the server should be stopped.
handle_write(Request :: term(), State :: term())
