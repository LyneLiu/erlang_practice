%% @author:lyne
%% @date:2015/10/12
%% @function:获取导出函数依赖模块列表。

-module(mod_exp_tackle).
% 调试状态
-compile(export_all).
% -export([exp_tackle/1]).


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
			Mod = erlang:element(3,erlang:element(3,Tuple)),
			Fun = erlang:element(3,erlang:element(4,Tuple)),
			Arity = erlang:length(RepList),
			case lists:member(Mod,ModList) orelse code:is_sticky(Mod) of
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
					case lists:member(Mod,ModList) orelse code:is_sticky(Mod) of
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
	NewModList = [Mod|ModList], 

	%% 检测Mod是否已经加载？没有加载的话使用新定义的c1()函数将其加载至VM中

	{FunForms,_,_} = meta:function(Name,Arity,Mod),
	{_,_,_,_,[{_,_,_,_,RepList}]} = FunForms,
	exp_tackle_recursive(RepList,NewModList).