-module(test_send).
-compile(export_all).

send()	->
	Fun = fun test_send:square/1,
	{test_receive,'b@172.21.218.44'} ! Fun.

square(X)	->
	X * X.