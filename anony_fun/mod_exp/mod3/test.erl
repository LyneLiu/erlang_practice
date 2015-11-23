-module(test).
-compile(export_all).

start()	->
	% FunVal1 = fun mod_exp1:fun1/0,
	% FunVal1().
	Fun = fun function_dependency:dep_fun/1,
	mod_exp_tackle:exp_tackle(Fun).

test_fun()	->
	Tuple = {name,lyne},
	case erlang:size(Tuple) of
		2	->
			ok;
		_Other	->
			false
	end.



