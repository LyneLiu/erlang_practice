-module(mod_exp3).
-compile(export_all).

fun3() 	->
	io:format("test 3.~n"),
	FunVal4 = fun mod_exp4:fun4/0,
	FunVal4().