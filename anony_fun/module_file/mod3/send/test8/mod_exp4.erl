-module(mod_exp4).
-compile(export_all).

fun4(Mod) 	->
	io:format("the function fun4 of module mod_exp4 is called by module ~p.~n",[Mod]).
