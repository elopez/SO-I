-module(fdserv).
-compile(export_all).

fd_server(Fd) ->
	receive
		{getfd, Pid} ->
			Pid ! Fd,
			fd_server(Fd+1);
		exit ->
			ok
	end.

get_fd(S) ->
	S ! {getfd, self()},
	receive Fd -> Fd end.

create() ->
	spawn(?MODULE, fd_server, [1]).

destroy(S) ->
	S ! exit.
