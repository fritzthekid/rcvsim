-module(rvscorehw).
-compile(export_all).

control(PIDRegs, Program, Data, PC) ->
    io:format("control: ~p~n",[PC]),
    Ret = maps:find(PC,maps:from_list(Program)),
    if 
	(Ret =:=error) ->
	    do_operation(self(), PIDRegs, ["nop"]);
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

do_operation(PIDCtl, PIDRegs, Op) ->
    % spawn(fun wait_a_sec/3, [PIDCtl,10,ok]),
    io:format("do_operation: ~p~n",[Op]),
    case hd(Op) of
	"mul" ->
	    io:format("Op: is mul~n",[]);
	"lw" ->
	    io:format("Op: is lw~n",[]);
	"slli" ->
	    io:format("~nOp: is slli~n",[]),
	    {DA,[S,E]} = {hd(lists:sublist(Op,3,1)),get_arguments(PIDRegs,Op,[2,4])},
	    io:format("~nstore: ~p pow(2,~p) = ~p->~p~n~n",[S,E,S*math:pow(2,E),DA]),
	    save_to_register(PIDRegs, DA, round(S * math:pow(2,E)));
	"add" ->
	    io:format("Op: is add~n",[]),
	    {DA,[S,O]} = {hd(lists:sublist(Op,3,1)),get_arguments(PIDRegs,Op,[2,4])},
	    save_to_register(PIDRegs, DA, S + O);
	"sw" ->
	    io:format("Op: is sw~n",[]);
	"addi" ->
	    io:format("Op: is addi~n",[]),
	    {DA,[S,O]} = {hd(lists:sublist(Op,3,1)),get_arguments(PIDRegs,Op,[2,4])},
	    save_to_register(PIDRegs, DA, S + O);
	"mv" ->
	    io:format("Op: is mv~n",[]);
	"store" ->  %% psydo instruction to store value to register
	    io:format("Op: is store~n",[]),
	    {DA,[O]} = {hd(lists:sublist(Op,2,1)),get_arguments(PIDRegs,Op,[3])},
	    save_to_register(PIDRegs, DA, O);
	_Else ->
	    io:format("Op: is unkown ~p~n",[_Else])
    end,
    timer:sleep(10),
    PIDCtl ! ok,
    ok.

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

