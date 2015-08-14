%% 
%% @author:lyne
%%
%% 问题描述：
%% 有两个列表，List1 = [{a，1}，{b，2} ，{c，3}，{b，1}]，
%% List2 = [{b，4}，{c，5}，{d，6}，{d，2}]，尝试将两个列表合并   
%% 最终得到的结果为：[{a，1}，{b，7}，{c，8}，{d，8}]
 

-module(list_demo).
-compile(export_all).

start()	->
	List1 = [{a,1},{b,2} ,{c,3},{b,1}],
	List2 = [{b,4},{c,5},{d,6},{d,2}],
	List = lists:keymerge(1,List1,List2),
	acc_list(List).


acc_list(List)	->
	acc_list(List,[]).

acc_list([{Var,Num} = Tuple|Rest],Acc)	->
	case lists:keyfind(Var,1,Acc) of
		false	->
			acc_list(Rest,[Tuple|Acc]);
		{Var,Num1}	->
			Num2 = Num + Num1,
			Acc1 = lists:keyreplace(Var,1,Acc,{Var,Num2}),
			acc_list(Rest,Acc1)
	end;
acc_list([],Acc)	->
	lists:reverse(Acc).


