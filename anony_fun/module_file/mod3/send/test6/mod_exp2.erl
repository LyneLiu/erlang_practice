-module(mod_exp2).
-compile(export_all).

fun2(Mod) 	->
	io:format("the function fun2 of module mod_exp2 is called by module ~p.~n",[Mod]),
	FunVal3 = fun mod_exp3:fun3/1,
	FunVal4 = fun mod_exp4:fun4/1,
	FunVal6_1 = fun mod_exp6:fun6_1/1,
	FunVal3(?MODULE),
	FunVal4(?MODULE),
	FunVal6_1(?MODULE).