-module(sender).
-compile(export_all).

send()	->
	Fun = fun mod_exp1:fun1/0,
	{receiver,'b@192.168.65.129'} ! Fun.


