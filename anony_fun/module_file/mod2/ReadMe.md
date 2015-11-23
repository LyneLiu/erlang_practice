1、模块说明
c模块：添加cplus函数，调用该函数编译erl源码文件时添加debug_debug参数；
function_tackle模块，处理函数信息；
meta、forms模块提取函数的Abstract Format；
smerl模块根据Abstract Format生成新的模块函数。

模块的位置？？？
通过make_preload脚本将其转换为硬编码？！


目前的模块：
meta、forms模块提取函数的Abstract Format；
smerl模块根据Abstract Format生成新的模块函数；
mod_exp_tackle模块提取导出函数的依赖模块列表；
mod_fun_tackle模块提取匿名函数的Abstract Format和ModList。

Note：将mod_exp_tackle和mod_fun_tackle两个合并为function_tackle模块，处理函数信息。


2、将所有相关的模块放置在module_file中。
c.erl
forms.erl
function_tackle.erl
meta.erl
smerl.erl
erlang.erl
rpc.erl
bif.c

错误信息：
message is export function.
** exception error: no function clause matching erlang:dsend(erlang,apply)


错误信息位置：bif.c文件中的do_send函数部分

解决：添加处理逻辑，目前的这个bug已经搞定了！

