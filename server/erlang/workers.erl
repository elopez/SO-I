-module(workers).
-compile(export_all).

-define(WORKER_QTY, 5).
-record(file, {name, owner, pos = 0, fd = 0, client, body = []}).
-record(worker, {lock, fd}).

main() ->
	Lock = lockserv:create(),
	Fd = fdserv:create(),
	State = #worker{lock = Lock, fd = Fd},
	First = spawn(?MODULE, circle_closer, []), % joiner process
	Last = spawn_chain(First, ?WORKER_QTY-1, State), % a chain of workers
	First ! {Last, State}, % join the chain ends
	First ! {trace, self(), []}, % and gather the list of all PIDs
	receive Workers -> Workers end.

% spawn a chain of interconnected workers
spawn_chain(Pid, 0, _) ->
	Pid;
spawn_chain(Pid, N, State) ->
	NPid = spawn(?MODULE, worker, [Pid, State, []]),
	spawn_chain(NPid, N-1, State).

% function to close a chain of interconnected workers, making a ring
circle_closer() ->
	receive {Pid, State} ->
		worker(Pid, State, [])
	end.

% worker process
worker(NPid, State, Files) ->
	MyPid = self(),
	Lock = State#worker.lock,
	Fds = State#worker.fd,
	receive
		% tracing commands
		{trace, Requester, [MyPid | N]} ->
			Requester ! [MyPid | N],
			worker(NPid, State, Files);
		{trace, Requester, N} ->
			NPid ! {trace, Requester, N ++ [self()]},
			io:format("Worker ~p initialized and ready for operation~n", [self()]),
			worker(NPid, State, Files);
		% exit
		exit ->
			NPid ! exit,
			io:format("Worker ~p exiting~n", [self()]),
			ok;
		% private worker commands
		{wln, Requester, Name} -> % worker link
			NFiles = Files ++ [#file{name=Name, owner=Requester}],
			NPid ! {wln, Requester, Name},
			worker(NPid, State, NFiles);
		{wop, Name, Fd} -> % worker open
			File = lists:keyfind(Name, #file.name, Files),
			NFile = File#file{fd=Fd},
			NFiles = lists:keyreplace(Name, #file.name, Files, NFile),
			NPid ! {wop, Name, Fd},
			worker(NPid, State, NFiles);
		{wde, Name} -> % worker delete
			NFiles = lists:keydelete(Name, #file.name, Files),
			NPid ! {wde, Name},
			worker(NPid, State, NFiles);
		% public worker commands
		{lsd, Requester, _} -> % list files
			Names = [X#file.name || X <- Files],
			Requester ! "OK " ++ string:join(Names, " ") ++ "\n",
			worker(NPid, State, Files);
		{cre, Requester, Name} -> % create a file
			case lockserv:try_lock(Lock) of
				ok ->
					case lists:keyfind(Name, #file.name, Files) of
						false ->
							NFiles = Files ++ [#file{name=Name, owner=MyPid}],
							Msg = {wln, MyPid, Name},
							NPid ! Msg,
							receive Msg -> ok end,
							Requester ! "OK\n";
						_ ->
							NFiles = Files,
							Requester ! "ERROR 17 EEXIST\n"
					end,
					lockserv:unlock(Lock),
					worker(NPid, State, NFiles);
				fail -> % requeue message
					self() ! {cre, Requester, Name},
					worker(NPid, State, Files)
			end;
		{opn, Requester, Name} -> % open a file
			case lists:keyfind(Name, #file.name, Files) of
				false ->
					Requester ! "ERROR 2 ENOENT\n",
					worker(NPid, State, Files);
				File ->
					case File#file.owner of
						MyPid when File#file.fd == 0 -> % our file is closed
							Fd = fdserv:get_fd(Fds),
							NFile = File#file{fd = Fd, client = Requester},
							NFiles = lists:keyreplace(Name, #file.name, Files, NFile),
							Msg = {wop, Name, Fd},
							NPid ! Msg,
							receive Msg -> ok end,
							Requester ! "OK FD "++integer_to_list(Fd)++"\n",
							worker(NPid, State, NFiles);
						MyPid -> % our file is opened
							Requester ! "ERROR 1 EPERM\n",
							worker(NPid, State, Files);
						Other -> % not our file, let the owner know
							Other ! {opn, Requester, Name},
							worker(NPid, State, Files)
					end
			end;
		{clo, Requester, [$F,$D,$ |Id]} -> % close a file
			Fd = list_to_integer(Id),
			case lists:keyfind(Fd, #file.fd, Files) of
				false ->
					Requester ! "ERROR 77 EBADFD\n",
					worker(NPid, State, Files);
				File ->
					case File#file.owner of
						MyPid when File#file.client == Requester -> % our file is opened by this client
							NFile = File#file{fd=0},
							NFiles = lists:keyreplace(Fd, #file.fd, Files, NFile),
							Msg = {wop, File#file.name, 0},
							NPid ! Msg,
							receive Msg -> ok end,
							Requester ! "OK\n",
							worker(NPid, State, NFiles);
						MyPid -> % our file is opened by some other client
							Requester ! "ERROR 1 EPERM\n",
							worker(NPid, State, Files);
						Other -> % not our file, let the owner know
							Other ! {clo, Requester, [$F,$D,$ |Id]},
							worker(NPid, State, Files)
					end
			end;
		{del, Requester, Name} -> % remove a file
			case lists:keyfind(Name, #file.name, Files) of
				false ->
					Requester ! "ERROR 2 ENOENT\n",
					worker(NPid, State, Files);
				File when File#file.fd =/= 0 ->
					Requester ! "ERROR 16 EBUSY\n",
					worker(NPid, State, Files);
				File ->
					case File#file.owner of
						MyPid -> % our file
							NFiles = lists:keydelete(Name, #file.name, Files),
							Msg = {wde, Name},
							NPid ! Msg,
							receive Msg -> ok end,
							Requester ! "OK\n",
							worker(NPid, State, NFiles);
						Other -> % not our file, let the owner know
							Other ! {del, Requester, Name},
							worker(NPid, State, Files)
					end
			end;
		% error catch-all
		{_, Requester, _} ->
			Requester ! "ERROR 71 EPROTO\n",
			worker(NPid, State, Files)
	end.
