-module(lockserv).
-compile(export_all).

lockState() ->
	receive
		{lock, Pid} ->
			Pid ! ok,
			be_locked(Pid);
		exit ->
			ok
	end.

be_locked(Pid) ->
	receive
		{lock, OtherPid} ->
			OtherPid ! fail,
			be_locked(Pid);
		{unlock, Pid} ->
			lockState()
	end.

lock(L) ->
	L ! {lock, self()},
	receive
		ok -> ok;
		fail -> lock(L)
	end.

try_lock(L) ->
	L ! {lock, self()},
	receive
		ok -> ok;
		fail -> fail
	end.

unlock(L) ->
	L ! {unlock, self()}.

create() ->
	spawn(?MODULE, lockState, []).

destroy(L) ->
	L ! exit.
