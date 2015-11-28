%% 导出函数依赖
-module(mod_exp1).
-compile(export_all).

fun1() 	->
	io:format("test 1.~n").
	Fun2 = fun mod_exp2:fun2/0,
	Fun3 = fun mod_exp3:fun3/0,
	Fun4 = fun mod_exp4:fun4/0,
	Fun2(),
	Fun3(),
	Fun4().