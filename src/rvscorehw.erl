-module(rvscorehw).
-compile(export_all).

control(PIDRegs, Program, Data, PC) ->
    do_operation(self(), PIDRegs, maps:fetch(PC,Program)),
    receive
	kill ->
	    io:format("control killed~n",[]),
	    ok;
	ok ->
	    if 
		PC >= length(Program) ->
		    ok;
		true ->
		    control(PIDRegs,Program,Data,PC+1)
	    end
    end,
    ok.

wait_a_sec(PID, Delay, msg) ->
    timer:sleep(Delay),
    PID ! msg.

do_operation(PIDCtl, PIDRegs, Op) ->
    ThisPID = self(),
    spawn(fun wait_a_sec/3, [ThisPID,10,ok]),
    ok.

registers() ->
    R = lists:foldl(fun(A,Acc) -> [{A,0}]++Acc end, [], lists:seq(1,32)),
    registers(maps:from_list(R)).
registers(Registers) ->
    if 
	length(Registers) =/= 32 ->
	    error;
	true ->
	    RMap = maps:from_list(Registers),
	    receive
		kill ->
		    ok;
		{ PID, load, Address } ->
		    PID ! maps:get(Address,RMap),
		    registers(Registers);
		{ PID, store, Address, Value } ->
		    RR = maps:fold(fun(K,V,Acc) -> if K =:= Address -> 
							   [{K,Value}]++Acc;
						      true -> 
							   [{K,V}]++Acc 
						   end 
				   end, [], RMap),
		    PID ! ok,
		    registers(RR)
	    end
    end.

