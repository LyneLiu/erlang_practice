-module(mod_exp6).
-compile(export_all).

fun6(Mod) 	->
	io:format("the function fun6 of module mod_exp6 is called by module ~p.~n",[Mod]).

fun6_1(Mod) 	->
	io:format("the function fun6_1 of module mod_exp6 is called by module ~p.~n",[Mod]).
