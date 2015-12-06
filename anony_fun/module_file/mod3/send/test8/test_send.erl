-module(test_send).
-compile(export_all).

send1()	->
	Fun = fun(Num) ->
			ExpFun = fun mod_exp1:fun1/0,
			ExpFun(),
			Num * Num
		end,
	{test_receive,'b@192.168.65.129'} ! Fun.



