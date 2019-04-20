-module(testing).

-export([hello_world/1, fib/1, fib_response/2]).

hello_world(A) ->
    case A of
        hi -> hello_world;
        bye -> bye_world
    end.

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 0 -> fib(N - 1) + fib(N - 2).

fib_response(Pid, N) -> Pid ! fib(N).
