-module(middle_server).
-export([start/3]).

start(MM) ->
    loop(MM).

loop(MM) ->
    receive
	{chan, MM, {add, M, S}} ->
	    polynomial:add(M, S),
	    loop(MM);
  {chan, MM, {sub, M, S}} ->
      polynomial:sub(M, S),
      loop(MM);
  {chan, MM, {mult, M, S}} ->
      polynomial:mult(M, S),
	    loop(MM);
  {chan, MM, {stop}} ->
      polynomial:stop();
	{chan_closed, MM} ->
	    true
    end.
