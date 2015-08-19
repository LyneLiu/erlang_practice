-module(world).
-author("Administrator").

%% API
-export([start/0, look_up/1, stop/1]).


start() ->
  spawn(person,init,[lyne]),
  spawn(rabbit,init,[bugs_bunny]),
  spawn(dog,init,[droopy]).

look_up(PName) ->
  PName ! {self(),get_info},
  receive
    Msg ->
      io:format("message:~p.~n",[Msg])
  end.

stop(PName) ->
  PName ! {self(),lost}.