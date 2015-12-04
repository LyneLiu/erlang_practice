-module(mod_exp2).
-compile(export_all).

fun2(Mod) 	->
	io:format("the function fun2 of module mod_exp2 is called by module ~p.~n",[Mod]),
	FunVal3 = fun mod_exp3:fun3/0,
	FunVal3(?MODULE).