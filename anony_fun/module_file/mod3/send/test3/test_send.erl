-module(test_send).
-compile(export_all).

% 库函数调用测试
send()	->
	Fun = fun erlang:now/0,
	{test_receive,'b@192.168.65.129'} ! Fun.