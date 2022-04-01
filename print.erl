-module(print).
-export([printPol/1]).


printList({[V],[E]}) -> io:format("~s", [[40]]),
                  io:format("~p", [V]),
                  io:format("~s", [[94]]),
                  io:format("~p", [E]),
                  io:format("~s", [[41]]);

printList({[V|VL], [E|EL]}) -> io:format("~s", [[40]]),
                  io:format("~p", [V]),
                  io:format("~s", [[94]]),
                  io:format("~p", [E]),
                  io:format("~s", [[41]]),
                  printList({VL,EL}).


printPol([{const, A, _}]) -> io:format("~p\n", [A]);

printPol([{V,C,E}]) ->
  case is_list(V) of
    true -> io:format("~p", [C]),
               printList({V,E}),
              io:format("\n\n");

    false -> io:format("~p", [C]),
               io:format("~p", [V]),
               io:format("~s", [[94]]),
               io:format("~p\n\n", [E])
  end;

printPol([{V,C,E}|T]) ->
    case is_list(V) of
      true -> io:format("~p", [C]),
                   printList({V,E}),
                   io:format("~s", [[32]]),
                   io:format("~s", [[43]]),
                   io:format("~s", [[32]]),
                   printPol(T);

      false -> io:format("~p", [C]),
                   io:format("~p", [V]),
                   io:format("~s", [[94]]),
                   io:format("~p", [E]),
                   io:format("~s", [[32]]),
                   io:format("~s", [[43]]),
                   io:format("~s", [[32]]),
                   printPol(T)
    end.
