%% date:2015/10/16
%% author:lyne
%% 这个demo是用来测试启动函数中含有三个receive语句时，
%% receive 语句的执行过程。
-module(receive_test).
-compile(export_all).

f1() ->
	receive
		_Msg -> io:format("f1 receive Msg.~n"),
		f1()   % 将f1作为loop执行循环
	end,
	f2(),
	f3().

f2() ->
	receive
		_Msg -> io:format("f2 receive Msg.~n")
	end.

f3() ->
	receive
		_Msg -> io:format("f3 receive Msg.~n")
	end.

start()	->
	register(test,spawn(receive_test,f1,[])).