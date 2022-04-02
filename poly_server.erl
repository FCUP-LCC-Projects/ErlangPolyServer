-module(poly_server).
-export([start/0, add/2,sub/2, mult/2, stop/0]).
-import(print, [printPol/1]).
-import(polynomial, [add_p/1, sub_p/1, mult_p/2, sort/1]).

start() -> register(polynomial, spawn(fun() -> loop() end)).

add(M,S) ->
    polynomial ! {add, self(), M, S},
    receive
      {ok, Result} -> print:printPol(Result), Result
    end.

sub(M, S) ->
    polynomial ! {sub, self(), M, S},
    receive
      {ok, Result} -> print:printPol(Result), Result
    end.

mult(M, S) ->
    polynomial ! {mult, self(), M, S},
    receive
      {ok, Result} -> print:printPol(Result), Result
    end.

stop() ->
   polynomial ! {stop, self()},
   ok.


loop() ->
    receive
      {add, From, M,S} ->  Sorted = polynomial:sort(M ++ S),
                              From ! {ok, polynomial:add_p(Sorted)},
                              loop();
      {sub,From, M,S} ->  Sorted = polynomial:sort(M ++ S),
                              From ! {ok, polynomial:sub_p(Sorted)},
                              loop();
      {mult,From, M,S} -> Sorted_M = polynomial:sort(M),
                          Sorted_S = polynomial:sort(S),
                              From ! {ok, polynomial:mult_p(Sorted_M, Sorted_S)},
                              loop();
      {stop, From} -> From ! close
    end.
