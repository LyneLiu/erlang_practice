%% @author:lyne
%% @date:2015/11/1
%% @function:1、获取导出函数依赖模块列表；2、处理匿名函数，获取Abstract Format和函数依赖模块列表

-module(function_tackle).
-export (
	[
	exp_tackle/2,
	fun_tackle/1
	]
		).

% 导出函数处理的接口
exp_tackle(ExpFun,ModList)	->
	NewModList = exp_tackle_1(ExpFun,ModList),
	[Mod1 || {Mod1,_,_} <- NewModList].

% ExpFun：导出函数
% ModList:导出函数的依赖模块列表
exp_tackle_1(ExpFun,ModList)	->
	FunForms = exp_abstract_info(ExpFun),
	{_,_,_,_,[{_,_,_,_,RepList}]} = FunForms,
	exp_tackle_recursive(RepList,ModList).

%% 处理RepList（函数表达式Format列表）
exp_tackle_recursive([HRep|TailRepList],ModList)	->
	Format = atom_to_list(erlang:element(1,HRep)),
	case Format of
		"call"	->
			NewModList = call_tackle(HRep,ModList),
			exp_tackle_recursive(TailRepList,NewModList);
		"match"	->
			NewModList = match_tackle(HRep,ModList),
			exp_tackle_recursive(TailRepList,NewModList);
		"case"	->
			NewModList = case_tackle(HRep,ModList),
			exp_tackle_recursive(TailRepList,NewModList);
		"if"	->
			NewModList = if_tackle(HRep,ModList),
			exp_tackle_recursive(TailRepList,NewModList);
		"clause"	->
			NewModList = clause_tackle(HRep,ModList),
			exp_tackle_recursive(TailRepList,NewModList);
		_Other ->
			exp_tackle_recursive(TailRepList,ModList)
	end;
exp_tackle_recursive([],ModList)	->
	ModList.

% call子句的处理
call_tackle(Rep,ModList)	->
	{_,_,Tuple,RepList} = Rep,
	SubClause =  atom_to_list(erlang:element(1,Tuple)),
	case SubClause of
		"remote"	->
			Mod = erlang:element(3,erlang:element(3,Tuple)),
			Fun = erlang:element(3,erlang:element(4,Tuple)),
			Arity = erlang:length(RepList),
			case code:is_sticky(Mod) orelse lists:member({Mod,Fun,Arity},ModList) of
				true ->
					ModList;
				false ->
					% 提取目标元素，并处理
					sub_exp_tackle(Mod,Fun,Arity,ModList)
			end;
		_Other	->
			exp_tackle_recursive(RepList,ModList)
	end.


% match子句的处理
match_tackle(Format,ModList)	->
	{_,_,_,Tuple} = Format,
	SubClause =  atom_to_list(erlang:element(1,Tuple)),
	case SubClause of
		"call" ->
			call_tackle(Tuple,ModList);
		"fun" ->
			fun_tackle(Tuple,ModList);
		_Other	->
			ModList
	end.

% case子句的处理
case_tackle(Format,ModList)	->
	{_,_,Tuple,RepList} = Format,
	SubClause =  atom_to_list(erlang:element(1,Tuple)),
	case SubClause of
		"call"	->
			NewModList = call_tackle(Tuple,ModList),
			sub_case_tackle(RepList,NewModList);
		_Other ->
			ModList
	end.


% if子句的处理
if_tackle(Format,ModList)	->
 	{_,_,RepList} = Format,
 	exp_tackle_recursive(RepList,ModList).

% clause子句的处理
clause_tackle(Format,ModList)	->
	{_,_,Rep1,[Rep2],Rep3} = Format,
	ModList1 = exp_tackle_recursive(Rep1,ModList),
	ModList2 = exp_tackle_recursive(Rep2,ModList1),
	exp_tackle_recursive(Rep3,ModList2).

% fun子句的处理
fun_tackle(Rep,ModList)	->
	{_,_,Tuple} = Rep,
	SubClause = atom_to_list(erlang:element(1,Tuple)),
	case SubClause of
		% 有两种情况的function子句
		"function" ->
			if
				erlang:size(Tuple) =:= 4 ->
					Mod = erlang:element(3,erlang:element(2,Tuple)),
					Fun = erlang:element(3,erlang:element(3,Tuple)),
					Arity = erlang:element(3,erlang:element(4,Tuple)),
					case code:is_sticky(Mod) orelse lists:member({Mod,Fun,Arity},ModList)  of
						true ->
							ModList;
						false ->
							sub_exp_tackle(Mod,Fun,Arity,ModList)
					end;
				true 	->
					ModList
			end;
		"clauses"	->
			{_,[{_,_,_,_,SubRepList}]} = Tuple,
			exp_tackle_recursive(SubRepList,ModList);
		_Other	->
			ModList
	end.

%% clause 处理
sub_case_tackle([HeadFormat|TailFormatList],ModList)	->
	{_,_,_,_,SubFormat} = HeadFormat,
	NewModList = exp_tackle_recursive(SubFormat,ModList),
	sub_case_tackle(TailFormatList,NewModList);
sub_case_tackle([],ModList)	->
	ModList.


%% 获取导出函数关于函数依赖的模块信息
exp_abstract_info(ExpFun)	->
	{module,Mod} = erlang:fun_info(ExpFun,module),
	{name,Name} = erlang:fun_info(ExpFun,name),
	{arity,Arity} = erlang:fun_info(ExpFun,arity),
	{FunForms,_,_} = meta:function(Name,Arity,Mod),
	FunForms.


% 子模块的导出函数依赖
sub_exp_tackle(Mod,Name,Arity,ModList)		->
	% 确保所有Mod编译加载至VM
	case erlang:module_loaded(Mod) of
		true ->
			io:format("~p module have been loaded!~n",[Mod]),
			ok;
		false 	->
			c:cplus(Mod)
	end,
	NewModList = [{Mod,Name,Arity}|ModList], 
	{FunForms,_,_} = meta:function(Name,Arity,Mod),
	{_,_,_,_,[{_,_,_,_,RepList}]} = FunForms,
	exp_tackle_recursive(RepList,NewModList).

%% -----------------------------------------------------------------------
%% 处理匿名函数，获取匿名函数的Abstract Format和依赖模块列表
fun_tackle(AnonyFun)	->
	{module,Mod} = erlang:fun_info(AnonyFun,module),
	{ExpName,Arity,FunCount} = anony_fun_info(AnonyFun),
	{FunInfo,_,_} = meta:function(ExpName,Arity,Mod),
	AnonyFunAbs = fun_abs_tackle(FunInfo,FunCount),
	ModList = exp_tackle_recursive([AnonyFunAbs],[Mod]),
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