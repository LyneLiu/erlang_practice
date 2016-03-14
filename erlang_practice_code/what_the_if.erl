-module(what_the_if).
-export([if_test/0]).

if_test()	->
	if
		1 =:= 1 ->
			io:format("1 works.~n")
	end,
	if
		1 =:= 2; 1 =:= 1 ->
			io:format("2 works.~n")
	end,
	if
		1 =:= 2, 1 =:= 1 ->
			io:format("3 works.~n")
	end.
