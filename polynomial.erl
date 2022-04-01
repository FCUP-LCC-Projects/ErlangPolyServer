-module(polynomial).
-export([start/0, add/2,sub/2, mult/2, stop/0, loop/0]).
-import('lists', [max/1, append/2, member/2]).
-import(print, [printPol/1]).

start() -> register(polynomial, spawn(fun() -> loop() end)).

coef({_, C, _}) -> C.

exp({_,_,[E|T]}) -> lists:max([E|T]);
exp({_, _, E}) -> E.

exp(_, {[], _, []}) -> [];
exp({V,C,E}, {[V|VL], C2, [E2|EL]}) -> NewExp = E+E2, [NewExp] ++ exp({V,C,E}, {VL,C2,EL});
exp({V,C,E}, {[_|VL], C2, [E2|EL]}) -> [E2] ++ exp({V,C,E}, {VL,C2,EL}).

variable({V,_,_}) ->V.

canOperate(F,S) -> VarEqual = variable(F) == variable(S),
                   ExpEqual = exp(F) == exp(S),
                   VarEqual and ExpEqual.

sort([H|T]) -> sort([E || E <- T, exp(E) > exp(H)]) ++ [H] ++ sort([E || E <- T, exp(E) =< exp(H)]);
sort([]) -> [].


add_s([H, N|T]) ->
  case canOperate(H,N) of
        true ->   NewCoef = coef(H)+coef(N),
                  add_p([{variable(H), NewCoef, exp(H)} | T]);

        false ->  [H] ++ add_p([N|T])
  end.


add_list([{A,C,E}, {A,C2,E}|T]) -> NewCoef = C+C2,
                              add_p([{A, NewCoef, E}|T]);
add_list([M,H|T]) -> [M]++add_p([H|T]).

add_p([H]) -> [H];
add_p([{V, C, E}, {VH, CH, EH}|T]) ->
  case is_list(V) and is_list(VH) of
    true -> add_list([{V, C, E}, {VH, CH, EH}|T]);
%% Even if one of them is a complex polynomial with multiple variables, the check to see if
%% they can be added will always fail at canOperate so there's no need for an extra condition

    false -> add_s([{V, C, E}, {VH, CH, EH}|T])
  end.


sub_s([H, N|T]) ->
  case canOperate(H,N) of
      true ->   NewCoef = coef(H)-coef(N),
                sub_p([{variable(H), NewCoef, exp(H)} | T]);

      false ->  [H] ++ sub_p([N|T])
  end.

sub_list([{A,C,E}, {A,C2,E}|T]) -> NewCoef = C-C2,
                            sub_p([{A, NewCoef, E}|T]).

sub_p([H]) -> [H];
sub_p([H,N|T]) ->
  case is_list(variable(H)) and is_list(variable(N)) of
    true -> sub_list([H,N|T]);

    false -> sub_s([H,N|T])
  end.


checkForConst({V,C,E}, {const, C2,_}) -> {V, C*C2, E};
checkForConst({const,C,_}, {V, C2,E2}) -> {V, C*C2, E2};
checkForConst({V,C,E}, {VL,C2,EL}) ->
  case is_list(VL) of
    true -> {lists:append([V], VL),C*C2,lists:append([E],EL)};

    false -> {[V,VL], C*C2, [E,EL]}
  end.


%%list to simple thats not already in list
mult_var_simple({V, C, E}, {VH, CH, EH}) -> [checkForConst({V, C, E}, {VH, CH, EH})].

%%list to simple thats on the list

mult_var_search({V,C,E}, {VL, C2, EL}) ->
                              NewCoef = C * C2,
                              NewExpL = exp({V,C,E}, {VL, C2, EL}),
                              [{VL, NewCoef, NewExpL}].

mult_var_check(M, H) ->
  case member(variable(M), variable(H)) of
    true -> mult_var_search(M,H);

    false -> mult_var_simple(M,H)
  end.


%%list to list

mult_var_list({[],C,[]},{VL2,C2,EL2}) ->
                              NewCoef = C*C2,
                              [{VL2, NewCoef, EL2}];

mult_var_list({[V|VL],C,[E|EL]}, {VL2, C2, EL2}) ->
    case member(V, VL2) of
      true -> NewExpL = exp({V,C,E}, {VL2, C2, EL2}),
              mult_var_list({VL,C,EL},{VL2,C2,NewExpL});

      false -> NewList = {lists:append([V], VL2), C2, lists:append([E], EL2)},
              mult_var_list({VL,C,EL},NewList)
    end.


mult_list({V,C,E}, {VH,CH,EH}) ->[checkForConst({V,C,E}, {VH,CH,EH}];

mult_list({V,C,E}, {V,CH,EH}) ->
            NewCoef = C*CH,
            NewExp = E+EH,
            [{V,NewCoef,NewExp}].


mult_r([], _) -> [];
mult_r([H|T], L) -> mult_aval(H, L) ++ mult_r(T, L).

mult_p(M, S) -> List = mult_r(M,S),
              Minimized = add_s(List),
              sort(Minimized).

mult_aval_f(M, H) ->
  case is_list(variable(H)) of
    true -> mult_var_list(M,H);

    false -> mult_var_check(H, M)
  end.

mult_aval_s(M,H) ->
  case is_list(variable(H)) of
    true -> mult_var_check(M, H);

    false -> mult_list(M,H)
  end.

mult_aval(_, []) -> [];
mult_aval(M, [H|L]) ->
    case is_list(variable(M)) of
      true -> mult_aval_f(M,H) ++ mult_aval(M, L);

      false -> mult_aval_s(M,H) ++ mult_aval(M, L)
    end.





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
      {add, From, M,S} ->  Sorted = sort(M ++ S),
                              From ! {ok, add_p(Sorted)},
                              loop();
      {sub,From, M,S} ->  Sorted = sort(M ++ S),
                              From ! {ok, sub_p(Sorted)},
                              loop();
      {mult,From, M,S} -> Sorted_M = sort(M),
                          Sorted_S = sort(S),
                              From ! {ok, mult_p(Sorted_M, Sorted_S)},
                              loop();
      {stop, _, _} -> ok
    end.
