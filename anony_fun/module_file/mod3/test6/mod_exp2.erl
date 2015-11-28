-module(mod_exp2).
-compile(export_all).

fun2() 	->
	io:format("test 2.~n"),
	FunVal3 = fun mod_exp3:fun3/0,
	FunVal4 = fun mod_exp4:fun4/0,
	FunVal6_1 = fun mod_exp6:fun6_1/0,
	FunVal3(),
	FunVal4(),
	FunVal6_1().