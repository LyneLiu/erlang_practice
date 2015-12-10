-module(sender).
-compile(export_all).

send()	->
	Fun = fun(Num) ->
			Num * Num * Num
		end,
	{receiver,'b@192.168.65.129'} ! Fun.