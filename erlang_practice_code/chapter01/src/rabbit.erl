-module(rabbit).
-author("Administrator").

%% API
-export([init/1]).
-include("global.hrl").

init(Name)  ->
  Rabbit = #rabbit{name = Name},
  register(Name,self()),
  io:format("create rabbit:~p.~n",[Rabbit]),
  loop(Rabbit).

loop(Rabbit)  ->
  receive
    {From,get_info} ->
      Name = Rabbit#rabbit.name,
      Age = Rabbit#rabbit.age,
      From ! [{name,Name},{age,Age}],
      loop(Rabbit);
    {_From,lost}  ->
      ok
  end.