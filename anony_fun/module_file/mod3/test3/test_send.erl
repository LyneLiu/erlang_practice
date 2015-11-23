-module(test_send).
-compile(export_all).

send1()	->
	Fun = fun function_tackle:fun_tackle/0,
	{test_receive,'b@172.21.218.44'} ! Fun.