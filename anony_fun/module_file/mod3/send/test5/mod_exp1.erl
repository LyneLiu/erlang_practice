%% 导出函数依赖
-module(mod_exp1).
-compile(export_all).

fun1() 	->
	io:format("start a circular dependency test.~n"),
	FunVal2 = fun mod_exp2:fun2/0,
	FunVal2(?MODULE).

fun1_1(Mod) ->
	io:format("the function fun1_1 of module mod_exp1 is called by module ~p.~n",[Mod]).