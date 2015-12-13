-module(sender).
-compile(export_all).

send()	->
	Fun = fun(Num) ->
			ExpFun = fun mod_exp1:fun1/0,
			ExpFun(),
			Num * Num
		end,
	{receiver,'b@192.168.65.129'} ! Fun.



