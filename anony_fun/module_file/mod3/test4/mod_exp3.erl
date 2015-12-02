-module(mod_exp3).
-compile(export_all).

fun3(Mod) 	->
	io:format("the function fun3 of module mod_exp3 is called by module ~p.~n",[Mod]).