-module(mod_exp5).
-compile(export_all).

fun5(Mod) 	->
	io:format("the function fun5 of module mod_exp5 is called by module ~p.~n",[Mod]),
	FunVal6 = fun mod_exp6:fun6/0,
	FunVal7 = fun mod_exp7:fun7/0,
	FunVal3_1 = fun mod_exp3:fun3_1/0,
	FunVal6(?MODULE),
	FunVal7(?MODULE),
	FunVal3_1(?MODULE).