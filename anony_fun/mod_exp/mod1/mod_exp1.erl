%% 导出函数依赖
-module(mod_exp1).
-compile(export_all).

fun1() 	->
	io:format("test 1.~n"),
	FunVal2 = fun mod_exp2:fun2/0,
	FunVal2().