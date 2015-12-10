-module(sender).
-compile(export_all).

send()	->
	Fun = fun test_send:square/1,
	{receiver,'b@192.168.65.129'} ! Fun.

square(X)	->
	X * X.