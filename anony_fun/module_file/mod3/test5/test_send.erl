-module(test_send).
-compile(export_all).

send()	->
	Fun = fun mod_exp1:fun1/0,
	{test_receive,'b@172.21.218.44'} ! Fun.



