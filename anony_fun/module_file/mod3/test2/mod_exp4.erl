-module(mod_exp4).
-compile(export_all).
-import(mod_exp1,[fun1_1/0]).

fun4() 	->
	io:format("test 4.~n"),
	mod_exp1:fun1_1().
