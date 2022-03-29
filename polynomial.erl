-module(polynomial).
-export([start/0, add/2,sub/2, mult/2, stop/0, loop/0]).

start() -> register(polynomial, spawn(fun() -> loop() end)).

coef({_, C, _}) -> C.
exp({_, _, E}) -> E.
variable({V,_,_}) ->V.
canOperate(F,S) -> VarEqual = variable(F) == variable(S),
                   ExpEqual = exp(F) == exp(S),
                   VarEqual and ExpEqual.

sort([H|T]) -> sort([E || E <- T, exp(E) > exp(H)]) ++ [H] ++ sort([E || E <- T, exp(E) =< exp(H)]);
sort([]) -> [].

addC([H]) -> [H];
addC([H, N|T]) ->
  case canOperate(H,N) of
        true ->   NewCoef = coef(H)+coef(N),
                  addC([{variable(H), NewCoef, exp(H)} | T]);

        false ->  [H] ++ addC([N|T])
  end.

subC([H]) -> [H];
subC([H, N|T]) ->
    case canOperate(H,N) of
          true ->   NewCoef = coef(H)-coef(N),
                    subC([{variable(H), NewCoef, exp(H)} | T]);

          false ->  [H] ++ subC([N|T])
    end.

mult_list(M, [H]) -> NewCoef = coef(M)*coef(H),
                    NewExp = exp(M)+exp(H),
                    [{variable(M),NewCoef,NewExp}];

mult_list(M, [H|L]) ->  NewCoef = coef(M)*coef(H),
                        NewExp = exp(M)+exp(H),
                        [{variable(M),NewCoef,NewExp}] ++ mult_list(M, L).

mult_r([], _) -> [];
mult_r([H|T], L) -> mult_list(H, L) ++ mult_r(T, L).

multC(M, S) -> List = mult_r(M,S),
              Minimized = addC(List),
              sort(Minimized).


add(M,S) ->
    polynomial ! {add, self(), M, S},
    receive
      {ok, Result} -> printPol(Result), Result
    end.

sub(M, S) ->
    polynomial ! {sub, self(), M, S},
    receive
      {ok, Result} -> printPol(Result), Result
    end.

mult(M, S) ->
    polynomial ! {mult, self(), M, S},
    receive
      {ok, Result} -> printPol(Result), Result
    end.

stop() ->
   polynomial ! {stop, self()},
   ok.

printPol([H]) -> io:format("~p", [coef(H)]),
                   io:format("~p", [variable(H)]),
                   io:format("~s", [[94]]),
                   io:format("~p\n", [exp(H)]);

printPol([H|T]) -> io:format("~p", [coef(H)]),
                   io:format("~p", [variable(H)]),
                   io:format("~s", [[94]]),
                   io:format("~p", [exp(H)]),
                   io:format("~s", [[32]]),
                   io:format("~s", [[43]]),
                   io:format("~s", [[32]]),
                   printPol(T).

loop() ->
    receive
      {add, From, M,S} ->  Sorted = sort(M ++ S),
                              From ! {ok, addC(Sorted)},
                              loop();
      {sub,From, M,S} ->  Sorted = sort(M ++ S),
                              From ! {ok, subC(Sorted)},
                              loop();
      {mult,From, M,S} -> Sorted_M = sort(M),
                                 Sorted_S = sort(S),
                              From ! {ok, multC(Sorted_M, Sorted_S)},
                              loop();
      {stop, _, _} -> ok
    end.
