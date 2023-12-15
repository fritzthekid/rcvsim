-module(rvscorehw).
-compile(export_all).

control(PIDM, Program, OpTab, Data, PC) ->
    Ret = maps:find(PC,maps:from_list(Program)),
    if
	(Ret =:= error) ->
	    do_operation(PIDM, OpTab, ["nop"]);
	true ->
	    {ok,Inst} = Ret,
	    do_operation(PIDM, OpTab, Inst)
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
		    control(PIDM,Program,OpTab,Data,PC+1)
	    end
    end,
    maps:get(registers,PIDM) ! {self(), dump},
    %TimeOutDump = 1000, 
    receive
	{ok,_Registers} ->
	    ok %dump(_Registers)
    %after
	    % TimeOutDump ->
	    % io:format("Dump Timeout~n",[])
    end,
    rvsmain:kill(maps:fold(fun(_,V,Acc)-> [V]++Acc end,[],maps:remove(main,PIDM))),
    maps:get(main,PIDM) ! ok.

do_operation(PIDM, OpTab, Op) ->
    IsMem = lists:member(hd(Op),dict:fetch_keys(OpTab)),
    if 
	IsMem ->
	    %io:format("Op: ~p~n",[Op]),
	    do_op(PIDM,Op,OpTab),
	    timer:sleep(10),
	    self() ! ok;
	true ->
	    %io:format("!!! n.s.y: ",[]),
	    %io:format("Op: ~p~n",[Op]),
	    case hd(Op) of
		"lw" ->
		    ok; %io:format("Op: is lw~n",[]);
		"sw" ->
		    ok; %io:format("Op: is sw~n",[]);
		"mv" ->
		    ok; %io:format("Op: is mv~n",[]);
		_Else ->
		    ok %io:format("Op: is unkown ~p~n",[_Else])
	    end,
	    %% timer:sleep(10),
	    self() ! ok
    end.
    %% ok.

do_op(PIDM,Op,OpTab) ->
    logger:info("Op: ~p",[Op]),
    [DR,AL,Pat] = dict:fetch(hd(Op),OpTab),
    {DA,Args} = {hd(lists:sublist(Op,DR,1)),get_arguments(PIDM,Op,AL)},
    save_to_register(PIDM, DA, do_pat(Pat,Args)).

do_pat(Pat,Args) ->
    logger:info("Pat: ~p, Args: ~p",[Pat,Args]),
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
		_Default ->
		    error
	    end
    end.

get(N,L) ->
    hd(lists:sublist(L,N,1)).

get_arguments(PIDM,Op,L) ->
    LL = lists:foldl(fun(X,Acc) -> Acc++lists:sublist(Op,X,1) end, [], L),
    lists:foldl(fun(A,Acc) ->
			if
			    is_number(A) ->
				% io:format("get_arguments: is_number ~p~n",[A]),
				Acc++[A];
			    is_list(A) ->
				% io:format("get_arguments: is_list ~p~n",[A]),
				TimeOutLoad=10,
				maps:get(registers,PIDM) ! {self(),load,A},
				receive
				    {ok,Val} ->
					Acc++[Val];
				    _ ->
					Acc++[error]
				after
				    TimeOutLoad ->
					io:format("TimeOutLoad~n",[]),
					timeout
				end
			end
		end, [],LL).

save_to_register(PIDM, DA, Val) ->
    maps:get(registers,PIDM) ! {self(), store, DA, Val},
    TimeOutSave = 10,
    receive
	ok ->
	    ok
    after
	TimeOutSave ->
	    io:format("TimeOutSave~n"),
	    timeout
    end.

dump(Registers) ->
    io:format("DumpRegisters: ~p~n",[Registers]),
    Registers.

registers(init,Size,Filling) ->
    registers(lists:foldl(fun(X,Acc) -> 
				  Acc++[{"a"++integer_to_list(X),Filling}] end,
			  [], lists:seq(1,Size))++[{"sp",0},{"s0",0}]).

registers(Registers) ->
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
	    %% io:format("registers load: ~p~n",[Address]),
	    PID ! {ok,maps:get(Address,RMap)},
	    registers(Registers);
	{ PID, store, Address, Value } ->
	    %% io:format("registers store: ~p: ~p~n",[Address,Value]),
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
	    io:format("registers timeout~n",[]),
	    timeout
    end.

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
register_timeout_test() ->
    PID=spawn(rvscorehw,registers,[init,32,0]),
    timer:sleep(1010),
    ?assert(is_process_alive(PID)=:=false),
    ok.
do_pat_test() ->
    ?assertEqual(17,do_pat([1],[17])),
    ?assertEqual(20,do_pat([1,2,addi],[17,3])),
    ?assert(error=:=rvscorehw:do_pat([1,2,xxx],[17,3])).
load_timeout_test() ->
    PID=spawn(fun()->timer:sleep(10) end),
    ?assertEqual(timeout,get_arguments(maps:from_list([{registers,PID}]),["a1"],[1])).
save_timeout_test() ->
    PID=spawn(fun()->timer:sleep(10) end),
    ?assertEqual(timeout,save_to_register(maps:from_list([{registers,PID}]),"a1",[1])).
dump_register_test() ->
    PIDReg = spawn(rvscorehw,registers,[init,32,0]),
    PIDReg ! {self(),dump},
    receive
	{ok,Registers} ->
	    dump(Registers)
    end,
    PIDReg ! kill.
-endif.
