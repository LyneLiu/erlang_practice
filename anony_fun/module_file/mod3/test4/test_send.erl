-module(test_send).
-compile(export_all).

send1()	->
	Fun1 = fun mod_exp1:fun1/0,
	Fun2 = fun mod_exp2:fun2/0,
	Fun3 = fun mod_exp3:fun3/0,
	Fun4 = fun mod_exp4:fun4/0,
	{test_receive,'b@172.21.218.44'} ! Fun1,
	{test_receive,'b@172.21.218.44'} ! Fun2,
	{test_receive,'b@172.21.218.44'} ! Fun3,
	{test_receive,'b@172.21.218.44'} ! Fun4,



