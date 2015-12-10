-module(sender).
-compile(export_all).

% 库函数调用测试
send()	->
	Fun = fun erlang:now/0,
	{receiver,'b@192.168.65.129'} ! Fun.