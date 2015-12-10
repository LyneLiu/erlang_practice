-module(receiver).
-compile(export_all).

start()	->
	register(receiver,spawn(receiver,loop,[])).

loop()	->
	receive 
		Fun  when is_function(Fun)	->
			io:format("result is :~p.~n",[Fun]),
			Fun();
		Other	->
			io:format("Other Message:~p.~n",[Other])
	end,
	loop().
