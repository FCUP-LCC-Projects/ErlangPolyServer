-module(middle_server).
-export([start/3]).

start(MM, ArgC, ArgS) ->
    loop(MM).

loop(MM) ->
    receive
	{chan, MM, {add, M, S}} ->
	    MM ! {send, polynomial:add(M, S)},
	    loop(MM);
  {chan, MM, {sub, M, S}} ->
      MM ! {send, polynomial:sub(M, S)},
      loop(MM);
  {chan, MM, {mult, M, S}} ->
      MM ! {send, polynomial:mult(M, S)},
	    loop(MM);
  {chan, MM, {stop}} ->
      MM ! {send, polynomial:stop()};
	{chan_closed, MM} ->
	    true
    end.
