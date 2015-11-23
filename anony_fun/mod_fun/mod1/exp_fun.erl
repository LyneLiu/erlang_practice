-module(exp_fun).
-compile(export_all).

%% 获取导出函数的Abstract Form
parse_abs(FunInfo,FunCount)	->
	{_,_,_,_,[{_,_,_,_,RepList}]} = FunInfo,
	RepList,
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