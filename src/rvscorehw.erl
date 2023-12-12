-module(rvscorehw).
-compile(export_all).

control(PIDRgegs, Program, Data, PC) ->
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

do_operation(PIDCtl, PIDRegs, Op) ->
    spawn(fun(PIDCtl)->timer.sleep(10), PIDCtl!ok end, [])
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
		    PID ! Ok,
		    registers(RR)
	    end
    end.

