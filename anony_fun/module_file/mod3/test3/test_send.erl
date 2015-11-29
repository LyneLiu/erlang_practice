-module(test_send).
-compile(export_all).

send1()	->
	Fun = fun erlang:now/0,
	{test_receive,'b@172.21.218.44'} ! Fun.