-module(test_send).
-compile(export_all).

send()	->
	Fun = fun(Num) ->
			Num * Num * Num
		end,
	{test_receive,'b@192.168.65.129'} ! Fun.