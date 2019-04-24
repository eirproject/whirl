-module(testing).

-export([hello_world/1, fib/1, fib_response/2, fib_server/0]).

hello_world(A) ->
    case A of
        hi -> hello_world;
        bye -> bye_world
    end.

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 0 -> fib(N - 1) + fib(N - 2).

fib_response(Pid, N) -> Pid ! fib(N).

fib_server() -> 
    receive
        {Pid, N} ->
            Pid ! fib(N),
            fib_server();
        exit ->
            false
    end.

counter_server(Counter) ->
    receive
        {get_counter, Pid} ->
            Pid ! Counter,
            counter_server(Counter);
        increment ->
            counter_server(Counter + 1);
        decrement ->
            counter_server(Counter - 1);
        exit ->
            false
    end.
