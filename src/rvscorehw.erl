-module(rvscorehw).
-compile(export_all).

control(PIDRegs, Program, OpTab, Data, PC) ->
    Ret = maps:find(PC,maps:from_list(Program)),
    if
	(Ret =:= error) ->
	    do_operation(PIDRegs, OpTab, ["nop"]);
	true ->
	    {ok,Inst} = Ret,
	    do_operation(PIDRegs, OpTab, Inst)
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
		    control(PIDRegs,Program,OpTab,Data,PC+1)
	    end
    end,
    PIDRegs ! {self(), dump},
    %TimeOutDump = 1000, 
    receive
	{ok,Registers} ->
	    dump(Registers)
    %after
	    % TimeOutDump ->
	    % io:format("Dump Timeout~n",[])
    end,
    rvsmain:kill([PIDRegs]),
    ok.

%% wait_for_reg(PID, Delay, val) ->
%%     Timeout = 10,
%%     receive
%% 	{ ok, Val } ->
%% 	    PID ! Val
%%     after
%% 	Timeout ->
%% 	    io:format("wait for reg ends with timeout~",[])
%%     end.

do_operation(PIDRegs, OpTab, Op) ->
    IsMem = lists:member(hd(Op),dict:fetch_keys(OpTab)),
    if 
	IsMem ->
	    io:format("Op: ~p~n",[Op]),
	    do_op(PIDRegs,Op,OpTab),
	    timer:sleep(10),
	    self() ! ok;
	true ->
	    io:format("!!! n.s.y: ",[]),
	    io:format("Op: ~p~n",[Op]),
	    case hd(Op) of
		"lw" ->
		    io:format("Op: is lw~n",[]);
		"sw" ->
		    io:format("Op: is sw~n",[]);
		"mv" ->
		    io:format("Op: is mv~n",[]);
		_Else ->
		    io:format("Op: is unkown ~p~n",[_Else])
	    end,
	    timer:sleep(10),
	    self() ! ok
    end.
    %% ok.

do_op(PIDRegs,Op,OpTab) ->
    [DR,AL,Pat] = dict:fetch(hd(Op),OpTab),
    {DA,Args} = {hd(lists:sublist(Op,DR,1)),get_arguments(PIDRegs,Op,AL)},
    save_to_register(PIDRegs, DA, do_pat(Pat,Args)).

do_pat(Pat,Args) ->
    if 
	(length(Pat) =:= 1) -> 
	    hd(Args);
	true ->
	    case lists:last(Pat) of
		add ->
		    hd(Args)+get(get(2,Pat),Args);
		mul ->
		    hd(Args)*get(get(2,Pat),Args);
		asbsl ->
		    hd(Args) bsl get(get(2,Pat),Args);
		addi ->
		    hd(Args)+get(get(2,Pat),Args);
		true ->
		    error
	    end
    end.

get(N,L) ->
    hd(lists:sublist(L,N,1)).

get_arguments(PIDRegs,Op,L) ->
    % io:format("get_arguments: Op ~p, L ~p~n",[Op,L]),
    LL = lists:foldl(fun(X,Acc) -> Acc++lists:sublist(Op,X,1) end, [], L),
    % io:format("get_arguments: LL ~p~n",[LL]),
    lists:foldl(fun(A,Acc) ->
			if
			    is_number(A) ->
				% io:format("get_arguments: is_number ~p~n",[A]),
				Acc++[A];
			    is_list(A) ->
				% io:format("get_arguments: is_list ~p~n",[A]),
				TimeOutLoad=1000,
				PIDRegs ! {self(),load,A},
				receive
				    {ok,Val} ->
					Acc++[Val];
				    _ ->
					Acc++[error]
				after
				    TimeOutLoad ->
					io:format("TimeOutLoad~n",[])
				end
			end
		end, [],LL).

save_to_register(PIDRegs, DA, Val) ->
    PIDRegs ! {self(), store, DA, Val},
    TimeOutSave = 1000,
    receive
	ok ->
	    RetVal = ok
    after
	TimeOutSave ->
	    io:format("TimeOutSave~n"),
	    RetVal = error
    end,
    RetVal.

dump(Registers) ->
    io:format("DumpRegisters: ~p~n",[Registers]).

%% registers() ->
%%     R = lists:foldl(fun(A,Acc) -> [{A,0}]++Acc end, [], lists:seq(1,32)),
%%     io:format("registers/0: ~p~n",[length(R)]),
%%     registers(R).
registers(Registers) ->
    %io:format("registers started~n",[]),
    TimeOut = 1000,
    RMap = maps:from_list(Registers),
    receive
	kill ->
	    io:format("registers killed~n",[]),
	    ok;
	{ PID, dump } ->
	    PID ! {ok,Registers},
	    registers(Registers);
	{ PID, load, Address } ->
	    io:format("registers load: ~p~n",[Address]),
	    PID ! {ok,maps:get(Address,RMap)},
	    registers(Registers);
	{ PID, store, Address, Value } ->
	    io:format("registers store: ~p: ~p~n",[Address,Value]),
	    RR = maps:fold(fun(K,V,Acc) -> if K =:= Address -> 
						   [{K,Value}]++Acc;
					      true -> 
						   [{K,V}]++Acc 
					   end 
			   end, [], RMap),
	    PID ! ok,
	    registers(RR)
    after
	TimeOut ->
	    io:format("registers timeout~n",[])
    end.

