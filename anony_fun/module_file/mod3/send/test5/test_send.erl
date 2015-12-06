-module(test_send).
-compile(export_all).

send()	->
	Fun = fun mod_exp1:fun1/0,
	{test_receive,'b@192.168.65.129'} ! Fun.



