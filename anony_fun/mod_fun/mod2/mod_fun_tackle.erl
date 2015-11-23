%% @author:lyne
%% @date:2015/11/6

-module(mod_fun_tackle).
%% 测试状态
-compile(export_all).

%% 处理匿名函数，获取匿名函数的Abstract Format和依赖模块列表
fun_tackle(AnonyFun)	->
	{module,Mod} = erlang:fun_info(AnonyFun,module),
	{ExpName,Arity,FunCount} = anony_fun_info(AnonyFun),
	{FunInfo,_,_} = meta:function(ExpName,Arity,Mod),
	AnonyFunAbs = fun_abs_tackle(FunInfo,FunCount),
	ModList = mod_exp_tackle:exp_tackle_recursive([AnonyFunAbs],[Mod]),
	{AnonyFunAbs,ModList}.

%% 在定义匿名函数的导出函数的Abstract Format中获取匿名函数的Abstract Format
fun_abs_tackle(FunInfo,FunCount)	->
	{_,_,_,_,[{_,_,_,_,RepList}]} = FunInfo,
	Fun = fun(Element)	->
				case erlang:element(1,Element) of
					match ->
						SubElement = erlang:element(4,Element),
						E =  atom_to_list(erlang:element(1,SubElement)) ,
						if
							E =:= "fun" ->
								true;
							true	->
								false 
						end;
					_	->
						false
				end
			end,
	TupleList = lists:filter(Fun,RepList),
	lists:nth(FunCount + 1,TupleList).


%% 获取匿名函数的相关数据信息
%% 返回:{ExpName,Arity,FunCount}
%% ExpName:定义匿名函数的导出函数名
%% Arity:导出函数的Arity
%% FunCount:匿名函数在导出函数中定义的位置
anony_fun_info(Fun)	->
	{name,Name} = erlang:fun_info(Fun,name),
	Name1 = atom_to_list(Name),
	[$-|T] = Name1,
	{Rest,ExpName} = exp_name(T),
	{Arity,Rest1} = arity_num(Rest),
	FunCount = fun_count(Rest1),
	{list_to_atom(ExpName),list_to_integer(Arity),list_to_integer(FunCount)}.


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
	arity_num(Str,[]).

arity_num([H|T],Acc)	->
	case H =/= $- of
		true ->
			arity_num(T,[H|Acc]);
		false	->
			{lists:reverse(Acc),T}
	end.

%% 匿名函数序数
fun_count(Str)	->
	"fun-" ++ Rest = Str,
	fun_count(Rest,[]).

fun_count([H|T],Acc)	->
	case H =/= $- of
		true	->
			fun_count(T,[H|Acc]);
		false	->
			lists:reverse(Acc)
	end.