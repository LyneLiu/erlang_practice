-module(test).
-compile(export_all).

start()	->
	FunVal1 = fun mod_exp1:fun1/0,
	FunVal1().
