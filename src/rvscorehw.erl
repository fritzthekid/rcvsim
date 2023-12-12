-module(rvscorehw).
-compile(export_all).

control(PIDRegs, Program, Data, PC) ->
    io:format("control: ~p~n",[PC]),
    Ret = maps:find(PC,maps:from_list(Program)),
    if 
	(Ret =:=error) ->
	    do_operation(self(), PIDRegs, {nop});
	true ->
	    {ok,Inst} = Ret,
	    do_operation(self(), PIDRegs, Inst)
    end,

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

wait_for_reg(PID, Delay, val) ->
    Timeout = 10,
    receive
	{ ok, Val } ->
	    PID ! Val
    after
	Timeout ->
	    io:format("wait for reg ends with timeout~",[])
    end.

do_operation(PIDCtl, PIDRegs, Op) ->
    % spawn(fun wait_a_sec/3, [PIDCtl,10,ok]),
    timer:sleep(10),
    PIDCtl ! ok,
    ok.

registers() ->
    R = lists:foldl(fun(A,Acc) -> [{A,0}]++Acc end, [], lists:seq(1,32)),
    io:format("registers/0: ~p~n",[length(R)]),
    registers(R).
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

