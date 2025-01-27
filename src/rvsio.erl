%% -------------------------------------------------------------------
%% @doc
%% This module handles input/output operations for the RISC-V Simulator.
%% It includes serial I/O operations and watchdog functionality.
%% -------------------------------------------------------------------

-module(rvsio).
-compile([export_all]).

%% Serial IO function to print messages to stdout
serialio_stdout() ->
    serialio_stdout(2000).

serialio_stdout(T) ->
    TimeOut = T,
    receive
        wakeup ->
            serialio_stdout(T);
        kill ->
            ok;
        {print, Message} ->
            logger:debug("stdout: ~p", [Message]),
            io:format("STDOUT>> ~s~n", [Message]),
            serialio_stdout(T);
        _Default ->
            logger:warning("stdout failed: ~p", [_Default])
    after
        TimeOut ->
            logger:notice("serial_stdout timeout"),
            ok
    end.

%% Watchdog function to monitor and wake up processes
watchdog(PIDL, Count) ->
    watchdog(PIDL, Count, 1000).

watchdog(PIDL, Count, T) ->
    TimeOut = T,
    receive
        ok ->
            ok
    after
        TimeOut ->
            lists:foreach(fun(PID) ->
                              PID ! wakeup
                          end, PIDL),
            if Count > 1 -> watchdog(PIDL, Count - 1, T);
               true -> lists:foreach(fun(PID) -> exit(PID, kill) end, PIDL)
            end
    end.

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").

%% Test for watchdog timeout
watchdog_timeout_test() ->
    PID = spawn(rvsio, watchdog, [[], 2, 20]),
    ?assert(is_process_alive(PID)),
    timer:sleep(100),
    ?assertEqual(false, is_process_alive(PID)).

%% Test for serial IO stdout
serialio_stdout_test() ->
    PIDio = spawn(rvsio, serialio_stdout, [20]),
    PIDwd = spawn(rvsio, watchdog, [[PIDio], 8, 15]),
    PIDio ! {print, binary:list_to_bin("test message")},
    timer:sleep(200),
    ?assertEqual(false, is_process_alive(PIDio)),
    ?assertEqual(false, is_process_alive(PIDwd)).

-endif.
