% author:lyne
-module(test_receive).
-compile(export_all).

start()	->
	register(test_receive,spawn(test_receive,loop,[])).

loop()	->
	receive 
		Fun  when is_function(Fun)	->
			io:format("result is :~p.~n",[Fun]);
		Other	->
			io:format("Other Message:~p.~n",[Other])
	end,
	loop().
