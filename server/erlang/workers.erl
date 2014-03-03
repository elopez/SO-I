-module(workers).
-compile(export_all).

-define(WORKER_QTY, 5).
-record(file, {name, owner, pos = 0, fd = 0, client, body = []}).

main() ->
	Lock = lockserv:create(),
	First = spawn(?MODULE, circle_closer, []), % joiner process
	Last = spawn_chain(First, ?WORKER_QTY-1, Lock), % a chain of workers
	First ! {Last, Lock}, % join the chain ends
	First ! {trace, self(), []}, % and gather the list of all PIDs
	receive Workers -> Workers end.

% spawn a chain of interconnected workers
spawn_chain(Pid, 0, _) ->
	Pid;
spawn_chain(Pid, N, Lock) ->
	NPid = spawn(?MODULE, worker, [Pid, Lock, []]),
	spawn_chain(NPid, N-1, Lock).

% function to close a chain of interconnected workers, making a ring
circle_closer() ->
	receive {Pid, Lock} ->
		worker(Pid, Lock, [])
	end.

% worker process
worker(NPid, Lock, Files) ->
	MyPid = self(),
	receive
		% tracing commands
		{trace, Requester, [MyPid | N]} ->
			Requester ! [MyPid | N],
			worker(NPid, Lock, Files);
		{trace, Requester, N} ->
			NPid ! {trace, Requester, N ++ [self()]},
			io:format("Worker ~p initialized and ready for operation~n", [self()]),
			worker(NPid, Lock, Files);
		% exit
		exit ->
			NPid ! exit,
			io:format("Worker ~p exiting~n", [self()]),
			ok;
		% private worker commands
		{wln, Requester, Name} -> % worker link
			NFiles = Files ++ [#file{name=Name, owner=Requester}],
			NPid ! {wln, Requester, Name},
			worker(NPid, Lock, NFiles);
		% public worker commands
		{lsd, Requester, _} ->
			Names = [X#file.name || X <- Files],
			Requester ! "OK " ++ string:join(Names, " ") ++ "\n",
			worker(NPid, Lock, Files);
		{cre, Requester, Name} ->
			case lockserv:try_lock(Lock) of
				ok ->
					case lists:keyfind(Name, #file.name, Files) of
						false ->
							NFiles = Files ++ [#file{name=Name, owner=self()}],
							Msg = {wln, self(), Name},
							NPid ! Msg,
							receive Msg -> ok end,
							Requester ! "OK\n";
						_ ->
							NFiles = Files,
							Requester ! "ERROR 17 EEXIST\n"
					end,
					lockserv:unlock(Lock),
					worker(NPid, Lock, NFiles);
				fail -> % requeue message
					self() ! {cre, Requester, Name},
					worker(NPid, Lock, Files)
			end;
		{opn, Requester, Name} ->
			case lists:keyfind(Name, #file.name, Files) of
				false ->
					Requester ! "ERROR 2 ENOENT\n",
					worker(NPid, Lock, Files);
				File ->
					Us = self(),
					case File#file.owner of
						Us when File#file.fd == 0 -> % our file is closed
							NFile = File#file{fd=5},
							NFiles = lists:keyreplace(Name, #file.name, Files, NFile),
							Requester ! "OK FD 5\n",
							worker(NPid, Lock, NFiles);
						Us -> % our file is opened
							Requester ! "ERROR 1 EPERM\n",
							worker(NPid, Lock, Files);
						Other -> % not our file, let the owner know
							Other ! {opn, Requester, Name},
							worker(NPid, Lock, Files)
					end
			end;
		% error catch-all
		{_, Requester, _} ->
			Requester ! "ERROR 71 EPROTO\n",
			worker(NPid, Lock, Files)
	end.
