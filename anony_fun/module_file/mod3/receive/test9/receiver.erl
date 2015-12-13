-module(receiver).
-compile(export_all).

start(Num)	->
	register(receiver,spawn(receiver,loop,[Num])).

loop(Num)	->
	receive 
		Fun  when is_function(Fun)	->
			io:format("result is :~p.~n",[Fun(Num)]);
		Other	->
			io:format("Other Message:~p.~n",[Other])
	end,
	loop(Num).

