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


3、添加一些测试用例进行实践检验
test1实验已经能够正常运行，并且cplus函数已经添加至VM中，但是调用需要通过M:F的方式。


错误：
为什么code:get_object_code(Mod)得到结果为error？
smerl模块添加的调试信息并没有输出，L：601、615
解决办法：重新恢复Ubuntu系统环境，怀疑是系统出现问题！
除了code_server模块，其他模块添加至source file中.


@author：lyne
@date：2015/11/25
4、昨天晚上遇到一个问题，发送节点发送消息后，只是显示ok，并没有进入dsend处理逻辑
今天在公司测试通过，怀疑是send节点的IP地址有问题，所以没有发送成功！

今天在公司测试，目前test1已经可以顺利进行。
思考：当远程节点不存在的时候会不会影响进程flag的状态？！


