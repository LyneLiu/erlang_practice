%%
%% @author:lyne

-module(anony_fun).
-compile(export_all).

%% 获取匿名函数的信息
anony_fun_info(Fun)	->
	{name,Name} = erlang:fun_info(Fun,name),
	export_fun(Name).

export_fun(Name)	->
	Name1 = atom_to_list(Name),
	[$-|T] = Name1,
	{Rest,ExpName} = exp_name(T),
	{Arity,Rest1} = arity_num(Rest),
	FunCount = fun_count(Rest1),
	{ExpName,Arity,FunCount}.

%%	导出函数
exp_name(Str)	->
	exp_name(Str,[]).

exp_name([H|T],Acc)	->
	case H =/= $/ of
		true	->
			exp_name(T,[H|Acc]);
		false	->
			{T,lists:reverse(Acc)}
	end.

%% 导出函数参数
arity_num(Str)	->
	arity_num_1(Str,[]).

arity_num_1([H|T],Acc)	->
	case H =/= $- of
		true ->
			arity_num_1(T,[H|Acc]);
		false	->
			{lists:reverse(Acc),T}
	end.

%% 匿名函数序数
fun_count(Str)	->
	"fun-" ++ Rest = Str,
	fun_count_1(Rest,[]).

fun_count_1([H|T],Acc)	->
	case H =/= $- of
		true	->
			fun_count_1(T,[H|Acc]);
		false	->
			lists:reverse(Acc)
	end.