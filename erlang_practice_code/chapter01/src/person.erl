-module(person).
-author("Administrator").

%% API
-export([init/1]).
-include("global.hrl").

init(Name)  ->
   Person = #person{name = Name},
  register(Name,self()),
  io:format("create person:~p.~n",[Person]),
  loop(Person).

loop(Person)  ->
  receive
    {From,get_info} ->
      Name = Person#person.name,
      Age = Person#person.age,
      From ! [{name,Name},{age,Age}],
      loop(Person);
    {_From,lost}  ->
      ok
  end.


