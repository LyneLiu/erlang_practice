-module(dog).
-author("Administrator").

%% API
-export([init/1]).
-include("global.hrl").

init(Name)  ->
  Dog = #dog{name = Name},
  register(Name,self()),
  io:format("create dog:~p.~n",[Dog]),
  loop(Dog).

loop(Dog)  ->
  receive
    {From,get_info} ->
      Name = Dog#dog.name,
      Age = Dog#dog.age,
      From ! [{name,Name},{age,Age}],
      loop(Dog);
    {_From,lost}  ->
      ok
  end.
