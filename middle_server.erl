-module(middle_server).
-import(poly_server, [add/2, sub/2, mult/2, stop/0]).
-export([start/3]).

start(MM, ArgC, ArgS) ->
    loop(MM).

loop(MM) ->
    receive
	{chan, MM, {add, M, S}} ->
	    MM ! {send, poly_server:add(M, S)},
	    loop(MM);
  {chan, MM, {sub, M, S}} ->
      MM ! {send, poly_server:sub(M, S)},
      loop(MM);
  {chan, MM, {mult, M, S}} ->
      MM ! {send, poly_server:mult(M, S)},
	    loop(MM);
  {chan, MM, {stop}} ->
      MM ! {send, poly_server:stop()};
	{chan_closed, MM} ->
	    true
    end.
