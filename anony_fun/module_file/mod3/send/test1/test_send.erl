-module(test_send).
-compile(export_all).

send()	->
	Fun = fun test_send:square/1,
	{test_receive,'b@192.168.65.129'} ! Fun.

square(X)	->
	X * X.