%% -------------------------------------------------------------------
%% @doc
%% This module handles input/output operations for the RISC-V Simulator.
%% It includes serial I/O operations and watchdog functionality.
%% -------------------------------------------------------------------

-module(rvsio).
-compile([export_all]).

%% -------------------------------------------------------------------
%% @doc
%% This module handles input/output operations for the RISC-V Simulator.
%% It includes serial I/O operations and watchdog functionality.
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc
%% Starts the serial I/O process with a default timeout.
%%
%% @spec serialio_stdout() -> ok
%% -------------------------------------------------------------------
serialio_stdout() ->
    serialio_stdout(2000).

%% -------------------------------------------------------------------
%% @doc
%% Handles serial I/O operations with a specified timeout.
%%
%% @spec serialio_stdout(integer()) -> ok
%% -------------------------------------------------------------------
serialio_stdout(T) ->
    TimeOut = T,
    receive
        wakeup ->
            serialio_stdout(T);
        kill ->
            ok;
        { print, Message } ->
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

%% -------------------------------------------------------------------
%% @doc
%% Starts the watchdog process with a specified list of PIDs and count.
%%
%% @spec watchdog(list(pid()), integer()) -> ok
%% -------------------------------------------------------------------
watchdog(PIDL, Count) ->
    watchdog(PIDL, Count, 1000).

%% -------------------------------------------------------------------
%% @doc
%% Handles watchdog functionality with a specified list of PIDs, count, and timeout.
%%
%% @spec watchdog(list(pid()), integer(), integer()) -> ok
%% -------------------------------------------------------------------
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

%% -------------------------------------------------------------------
%% @doc
%% Tests the watchdog timeout functionality.
%%
%% @spec watchdog_timeout_test() -> ok
%% -------------------------------------------------------------------
watchdog_timeout_test()->
    PID = spawn(rvsio, watchdog, [[], 2, 20]),
    ?assert(is_process_alive(PID)),
    timer:sleep(100),
    ?assertEqual(false, is_process_alive(PID)).

%% -------------------------------------------------------------------
%% @doc
%% Tests the serial I/O stdout functionality.
%%
%% @spec serialio_stdout_test() -> ok
%% -------------------------------------------------------------------
serialio_stdout_test()->
    PIDio = spawn(rvsio, serialio_stdout, [20]),
    PIDwd = spawn(rvsio, watchdog, [[PIDio], 8, 15]),
    PIDio ! {print, binary:list_to_bin("test message")},
    timer:sleep(200),
    ?assertEqual(false, is_process_alive(PIDio)),
    ?assertEqual(false, is_process_alive(PIDwd)).
-endif.
