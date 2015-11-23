%% 匿名函数
-module(mod_fun).
-compile(export_all).

fun1()	->
	_Fun1  = 
		fun() 	-> io:format("test 1.~n") end,
	_Fun2  =
		fun(Num) 	-> 
			Fun3 = fun mod_fun:square/1,
			Fun3(Num),
			io:format("test 2.~n") end,
	anony_fun:fun_clause(_Fun2).

square(Num)	->
	Num * Num.



