-module(dispatcher).
-compile(export_all).

main() ->
	WList = workers:main(),
	case gen_tcp:listen(8000, [list, {active, false}, {reuseaddr, true}]) of
		{ok, LSocket} -> dispatcher(LSocket, 1, WList);
		_             -> io:format("Error creating listening socket.~n")
	end,
	receive exit -> ok end. % wait until we're asked to close

dispatcher(LSocket, Id, WList) ->
	{Worker, NWList} = next_in_line(WList),
	case gen_tcp:accept(LSocket) of
		{ok, CSocket} -> spawn(?MODULE, dispatcher, [LSocket, Id+1, NWList]),
				      io:format("Client #~p connected.~n", [Id]),
				      handle_client(CSocket, Id, [], 0, Worker);
		_             -> dispatcher(LSocket, Id, WList)
	end.

handle_client(CSocket, Id, Buf, Verified, Worker) ->
	case get_line(CSocket, Buf) of
		{ok, Line, NBuf} ->
			{Atom, Args} = str_to_atom(Line),
			case Atom of
				error ->
					gen_tcp:send(CSocket, "ERROR 71 EPROTO\n"),
					gen_tcp:close(CSocket);
				con when Verified == 0 ->
					gen_tcp:send(CSocket, "OK ID "++integer_to_list(Id)++[$\n]),
					handle_client(CSocket, Id, NBuf, 1, Worker);
				_ when Verified == 0 ->
					gen_tcp:send(CSocket, "ERROR 71 EPROTO\n"),
					gen_tcp:close(CSocket);
				_ when Verified == 1 ->
					Worker ! {Atom, self(), Args},
					receive X -> gen_tcp:send(CSocket, X) end,
					handle_client(CSocket, Id, NBuf, 1, Worker)
			end;
		_ ->
			io:format("Client #~p has left.~n", [Id])
	end.


% converts an [a-zA-Z]+ string to lowercase
alphalower(X) when X >= $A, X =< $Z -> (X-$A)+$a;
alphalower(X) when X >= $a, X =< $z -> X.

% converts "ABC" -> abc
str_to_atom([A, B, C | Z]) ->
	S = alphalower(A),
	T = alphalower(B),
	R = alphalower(C),
	Atom = list_to_atom([S, T, R]),
	case Z of
		[] -> {Atom, []};
		[$ | Arg] -> {Atom, Arg};
		_ -> {error, []}
	end;
str_to_atom(_) -> {error, []}.

% separate a buffer into lines
get_line(Sock, Buffer) ->
	case break_line(Buffer) of
		more -> % no newline on buffer
		    case gen_tcp:recv(Sock, 0) of
			{ok, Chars} -> % ok, got more chars
			    NBuf = Buffer++Chars,
			    case break_line(NBuf) of % do we have a line now?
				more -> get_line(Sock, NBuf); % nope :(
				Result -> Result % yep :)
			    end;
			_ -> error
		    end;
		Result -> Result % the buffer had a newline already
	    end.

% split a line after \n
break_line(Chars) ->
	break_line(Chars, [], 0).

break_line([$S,$I,$Z,$E,$ |Rest], Buf, 1) ->
	{Size, [$ | NRest]} = string:to_integer(Rest),
	case length(NRest) of
		N when N =< Size ->
			more;
		_ when Size == 0 ->
			NBuf = "SIZE 0 ",
			break_line(NRest, lists:reverse(NBuf) ++ Buf, 0);
		_ ->
			Queued = string:substr(NRest, Size+1),
			Content = string:substr(NRest, 1, Size),
			NBuf = "SIZE " ++ integer_to_list(Size) ++ " " ++ Content,
			break_line(Queued, lists:reverse(NBuf) ++ Buf, 0)
	end;
break_line([$W,$R,$T | Chars], Buf, 0) ->
	break_line(Chars, "TRW" ++ Buf, 1);
break_line([$\n | Chars], Buf, 0) ->
	{ok, lists:reverse(Buf), Chars};
break_line([X | Chars], Buf, S) ->
	break_line(Chars, [X | Buf], S);
break_line([], _, _) ->
	more.

% returns the first element and updates the list
next_in_line([X | Y]) -> {X, Y ++ [X]}.
