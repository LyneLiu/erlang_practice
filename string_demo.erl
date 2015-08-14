-module(string_demo).
-compile(export_all).

% 将字符串转化为Erlang数据类型

string2value(Str)	->
	{ok,Tokens,_} = erl_scan:string(Str ++ "."),
	{ok,Exprs} = erl_parse:parse_exprs(Tokens),
	Bindings = erl_eval:new_bindings(),
	{value,Value,_} = erl_eval:exprs(Exprs,Bindings),
	Value.