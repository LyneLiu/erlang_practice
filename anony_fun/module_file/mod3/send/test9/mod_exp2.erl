-module(mod_exp2).
-compile(export_all).

fun2(Mod) 	->
	io:format("fun2 of mod_exp2 called by ~p.~n",[Mod]),
	FunVal3 = fun mod_exp3:fun3/1,
	FunVal3(?MODULE).