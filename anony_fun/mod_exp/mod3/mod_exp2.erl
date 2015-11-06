-module(mod_exp2).
-compile(export_all).

fun2() 	->
	io:format("test 2.~n"),
	FunVal3 = fun mod_exp3:fun3/0,
	FunVal3().