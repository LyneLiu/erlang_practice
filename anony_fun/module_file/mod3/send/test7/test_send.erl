-module(test_send).
-compile(export_all).

send()	->
	Fun = fun(Num) ->
			Num * Num * Num
		end,
	{test_receive,'b@172.21.218.44'} ! Fun.