%% 导出函数依赖
-module(mod_exp1).
-compile(export_all).

fun1() 	->
	io:format("start a cross dependence test.~n"),
	FunVal2 = fun mod_exp2:fun2/0,
	FunVal5 = fun mod_exp5:fun5/0,
	FunVal2(?MODULE),
	FunVal5(?MODULE).