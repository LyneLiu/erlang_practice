-module(meta_mod).
-compile(export_all).

%% 测试
test1()	->
	Form = {function,1,start,0,[{clause,1,[],[],[{atom,1,ok}]}]},
    meta_fun(Form).

test2()	->
	_Fun = test,
	_Fun1 = fun() -> hello end,
	_Fun2 = fun() -> world end,
	_Fun3 = fun meta_mod:start/0,
	extract_fun(_Fun2).


extract_fun(Fun)	->
	Form = mod_fun_tackle:fun_tackle(Fun).
	% meta_fun(Form).

%% 生成temp_mod模块，并添加函数
%% 返回temp_mod模块的匿名函数
meta_fun(Form)	->
	M = smerl:new('temp_mod'),
	Forms = {function,1,start,0,[{clause,1,[],[],[Form]}]},
	{ok,M2} = smerl:add_func(M,Forms,true),
	smerl:compile(M2),
	temp_mod:start().	