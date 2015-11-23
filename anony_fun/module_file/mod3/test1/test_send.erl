-module(test_send).
-compile(export_all).

send1()	->
	send_message1(test_receive,'b@172.21.218.44').

send2()	->
	send_message2(test_receive,'b@172.21.218.44').

send_message1(Name,Node)	->
	Pid1 = spawn(test_send,send_square,[Name,Node]),
	io:format("Pid 1:~p.~n",[Pid1]).


send_message2(Name,Node)	->
	Pid2 = spawn(test_send,send_cube,[Name,Node]),
	io:format("Pid 2:~p.~n",[Pid2]).

send_square(Name,Node)	->
	Fun =fun test_send:square/1,
	{Name,Node} ! Fun.

square(X)	->
	X * X * 2.

send_cube(Name,Node)	->
	Fun = fun(X) ->
		X * X * X * 2 
	      end,
	{Name,Node} ! Fun.
