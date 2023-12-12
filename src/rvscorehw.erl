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
	    io:format("Op: is slli~n",[]);
	"add" ->
	    io:format("Op: is add~n",[]),
	    {D,[S,O]} = {hd(lists:sublist(Op,3,1)),get_arguments(PIDRegs,Op,[2,4])},
	    save_to_register(PIDRegs, D, S + O);
	"sw" ->
	    io:format("Op: is sw~n",[]);
	"addi" ->
	    io:format("Op: is addi~n",[]),
	    {D,[S,O]} = {hd(lists:sublist(Op,3,1)),get_arguments(PIDRegs,Op,[2,4])},
	    save_to_register(PIDRegs, D, S + O);
	"mv" ->
	    io:format("Op: is mv~n",[]);
	_Else ->
	    io:format("Op: is unkown ~p~n",[_Else])
    end,
    timer:sleep(10),
    PIDCtl ! ok,
    ok.

get_arguments(PIDRegs,Op,L) ->
    io:format("get_arguments: Op ~p, L ~p~n",[Op,L]),
    LL = lists:foldl(fun(X,Acc) -> lists:sublist(Op,X,1)++Acc end, [], L),
    io:format("get_arguments: LL ~p~n",[LL]),
    lists:foldl(fun(A,Acc) ->
			if
			    is_number(A) ->
				io:format("get_arguments: is_number ~p~n",[A]),
				Acc++[A];
			    is_list(A) ->
				io:format("get_arguments: is_list ~p~n",[A]),
				PIDRegs ! {self(),load,A},
				receive
				    {ok,Val} ->
					Acc++[Val];
				    _ ->
					Acc++[error]
				end
			end
		end, [],LL).

save_to_register(_PIDRegs, _D, _Val) ->
    ok.

%% registers() ->
%%     R = lists:foldl(fun(A,Acc) -> [{A,0}]++Acc end, [], lists:seq(1,32)),
%%     io:format("registers/0: ~p~n",[length(R)]),
%%     registers(R).
registers(Registers) ->
    io:format("registers started~n",[]),
    TimeOut = 1000,
    RMap = maps:from_list(Registers),
    receive
	kill ->
	    io:format("registers killed~n",[]),
	    ok;
	{ PID, load, Address } ->
	    io:format("registers load: ~p~n",[Address]),
	    PID ! {ok,maps:get(Address,RMap)},
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
    after
	TimeOut ->
	    io:format("registers timeout~n",[])
    end.

