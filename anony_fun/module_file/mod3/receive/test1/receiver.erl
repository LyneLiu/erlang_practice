-module(receiver).
-compile(export_all).

start(Num)	->
	register(receiver,spawn(receiver,loop,[Num])).

loop(Num)	->
	receive 
		Fun 	->
			io:format("message is :~p.~n",[Fun]),
			io:format("result is :~p.~n",[Fun(Num)])
	end,
	loop(Num).
