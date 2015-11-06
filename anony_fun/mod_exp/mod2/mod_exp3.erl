-module(mod_exp3).
-compile(export_all).

fun3() 	->
	io:format("test 3.~n"),
	LoopFun = fun mod_exp1:fun1_1/0,
	LoopFun().