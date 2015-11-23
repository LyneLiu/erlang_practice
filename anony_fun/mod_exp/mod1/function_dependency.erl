-module(function_dependency).
-compile(export_all).

%% 问题：Fun是发送到receiver节点的导出函数消息，
%% 当我们将Fun函数消息发送至对方节点的时候，我们需要
%% 考虑导出函数依赖关系，将相关Mod列表进行同步！


%% 获取导出函数关于函数依赖的模块信息
dep_mod_info(ExpFun)	->
	{module,Mod} = erlang:fun_info(ExpFun,module),
	{name,Name} = erlang:fun_info(ExpFun,name),
	{arity,Arity} = erlang:fun_info(ExpFun,arity),
	{FunForms,_,_} = meta:function(Name,Arity,Mod),
	dep_fun(FunForms).

%% 获取导出函数依赖
dep_fun(FunForms)	->
	{_,_,_,_,[{_,_,_,_,RepList}]} = FunForms,
	FoldFun = fun(Element,Acc)	->
			case  erlang:element(1,Element) of
				match ->
					SubElement = erlang:element(4,Element),
					E =  atom_to_list(erlang:element(1,SubElement)) ,
					if
						E =:= "fun" ->
							case erlang:element(1,erlang:element(3,SubElement)) of
								function ->
									FunInfo = get_fun_info(SubElement),
									[FunInfo|Acc];
								_ 	->
									Acc
							end;
						true	->
							Acc
					end;
				_ 	->
					Acc
			end
		end,
	lists:foldl(FoldFun,[],RepList).

%% 获取Mod、Fun、Arity
get_fun_info(Element)	->
	{_,_,{_,{_,_,Mod},{_,_,Fun},{_,_,Arity}}} = Element,
	{Mod,Fun,Arity}.

%% 判断是否存在新的函数依赖关系
deep_dep([Head|Tail],ModList)	->
	{Mod,Name,Arity} = Head,
	{FunForms,_,_} = meta:function(Name,Arity,Mod),
	Fun = fun(Element,Acc)	->
			case Element =:= Mod of
				true ->
					Acc1 = false,
					Acc1;
				false ->
				    Acc
			end
		end,
	Result = lists:foldl(Fun,false,ModList),
	%% 如果Mod已存在的话，不需要将Mod添加至ModList中
	case Result of
		true ->
			deep_dep(Tail,ModList);
		false ->
		    NewModList = [Mod|ModList],
		    NewModInfoList = dep_fun(FunForms),
		    NewModInfoList1 = NewModInfoList ++ Tail,
			deep_dep(NewModInfoList1,NewModList)
	end;
deep_dep([],ModList)	->
	ModList.

%% 测试用例
test1()	->
	Fun = fun test:start/0,
	ModInfoList = dep_mod_info(Fun),
	deep_dep(ModInfoList,[test]).
