-module(mod_exp4).
-compile(export_all).
-import(mod_exp1,[fun1_1/0]).

fun4() 	->
	io:format("test 4.~n"),
	mod_exp1:fun1_1().

% fun4(NumList) 	->
% 	Num = erlang:list_to_integer(NumList),
% 	case erlang:is_integer(Num) of
% 		true 	->
% 			if
% 				Num > 0 ->
% 					mod_exp1:fun1_1();
% 				true	->
% 					ok
% 			end;
% 		_Other 	->
% 			ok
% 	end.