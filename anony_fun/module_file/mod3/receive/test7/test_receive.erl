% author:lyne
-module(test_receive).
-compile(export_all).

start(Num)	->
	register(test_receive,spawn(test_receive,loop,[Num])).

loop(Num)	->
	receive 
		Fun 	->
			io:format("message is :~p.~n",[Fun]),
			io:format("result is :~p.~n",[Fun(Num)]),
	end,
	loop(Num).
