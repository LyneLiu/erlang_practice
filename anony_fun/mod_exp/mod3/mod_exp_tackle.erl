%% @author:lyne
%% @date:2015/10/12
%% @function:获取导出函数依赖模块列表。

-module(mod_exp_tackle).
-export([exp_tackle/1]).

%% 使用进程处理的方式获取到导数依赖模块的列表


% 导出函数处理的接口
exp_tackle(ExpFun)	->
	{module,Mod} = erlang:fun_info(ExpFun,module),
	exp_tackle(ExpFun,[Mod]).

% ExpFun：导出函数
% ModList:导出函数的依赖模块列表
exp_tackle(ExpFun,ModList)	->
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
			Acc = erlang:element(3,erlang:element(3,Tuple)),
			case lists:member(Acc,ModList) orelse code:is_sticky(Acc) of
				true ->
					ModList;
				false ->
					[Acc|ModList]
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
					SubTuple = erlang:element(2,Tuple),
					{_,_,Acc} = SubTuple,
					case lists:member(Acc,ModList) orelse code:is_sticky(Acc) of
						true ->
							ModList;
						false ->
							[Acc|ModList]
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

