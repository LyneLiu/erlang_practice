-module(mod_start).
-compile(export_all).

start()	->
	_Fun = fun meta_mod:test1/0,
	c:c(forms),
	c:c(meta),
	c:c(smerl).