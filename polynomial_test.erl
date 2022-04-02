-module(polynomial_test).
-compile(export_all).

test() ->
  poly_server:start(),
  poly_server:add([{[x,y],1,[2,2]},{x,2,4},{y,10,1},{const,-10,0}],[{[x,y],2,[2,2]},{[x,z],10,[3,2]},{y,-5,1},{const,10,0}]),
  poly_server:sub([{[x,y],1,[2,2]},{x,2,4},{y,10,1},{const,-10,0}],[{[x,y],2,[2,2]},{[x,z],10,[3,2]},{y,-5,1},{const,10,0}]),
  poly_server:mult([{[x,y],1,[2,2]},{x,2,4},{y,10,1},{const,-10,0}],[{[x,z],10,[3,2]},{y,-5,1},{const,10,0}]),
  poly_server:stop().

test_server() ->
  poly_server:start(),
  lib_chan:start_server().

test_client() ->
  {ok, Pid} = lib_chan:connect("localhost",1234,middleServer,"PC20212022",""),
  lib_chan:rpc(Pid, {add, [{x, -2, 1},{x, -1, 1},{x, 4, 5}],[{x, -3, 5},{x, 10, 3}, {x,2,4}]}),
  lib_chan:rpc(Pid, {mult, [{[x,y],1,[2,2]},{x,2,4},{y,10,1},{const,-10,0}],[{[x,z],10,[3,2]},{y,-5,1},{const,10,0}]}).
