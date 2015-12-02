-module(mod_exp7).
-compile(export_all).

fun7(Mod) 	->
	io:format("the function fun7 of module mod_exp7 is called by module ~p.~n",[Mod]).
