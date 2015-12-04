-module(test_send).
-compile(export_all).

% 库函数调用测试
send()	->
	Fun = fun erlang:now/0,
	{test_receive,'b@172.21.218.44'} ! Fun.