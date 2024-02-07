-module(rvsio).
-compile([export_all]).

serialio_stdout() ->
    serialio_stdout(2000).
serialio_stdout(T) ->
    TimeOut = T,
    receive
	wakeup ->
	    serialio_stdout(T);
	kill ->
	    ok;
	{ print, message } ->
	    io:format("~s~n",[message]),
	    serialio_stdout(T)
    after
	TimeOut ->
	    logger:notice("serial_stdout timeout"),
	    ok
    end.

watchdog(PIDL,Count) ->
    watchdog(PIDL,Count,1000).
watchdog(PIDL,Count,T) ->
    TimeOut = T,
    receive
	ok ->
	    ok
    after
	TimeOut ->
	    lists:foreach(fun(PID) ->
				  PID ! wakeup
			  end, PIDL),
	    if Count > 1 -> watchdog(PIDL,Count-1,T);
	       true -> lists:foreach(fun(PID)->exit(PID,kill) end,PIDL)
	    end
    end.

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
watchdog_timeout_test()->
    PID = spawn(rvsio,watchdog,[[],2,20]),
    ?assert(is_process_alive(PID)),
    timer:sleep(100),
    ?assertEqual(false,is_process_alive(PID)).
serialio_stdout_test()->
    PIDio = spawn(rvsio,serialio_stdout,[20]),
    PIDwd = spawn(rvsio,watchdog,[[PIDio],8,15]),
    PIDio ! {print,binary:list_to_bin("test message")},
    timer:sleep(200),
    ?assertEqual(false,is_process_alive(PIDio)),
    ?assertEqual(false,is_process_alive(PIDwd)).
-endif.

