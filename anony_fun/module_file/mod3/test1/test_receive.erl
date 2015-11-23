% author:lyne
-module(test_receive).
-compile(export_all).

start(Num)	->
	register(test_receive,spawn(test_receive,loop,[Num])).

loop(Num)	->
	receive 
		Fun 	->
			Result = Fun(Num),
			io:format("result is :~p.~n",[Result])
	end,
	loop(Num).
