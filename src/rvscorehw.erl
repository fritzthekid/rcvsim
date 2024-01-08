-module(rvscorehw).
-compile(export_all).

control(PIDM, Program, OpTab, Globals, Data, PC) ->
    case maps:find(PC,maps:from_list(Program)) of
	{ok, Inst} ->
	    do_operation(PIDM, OpTab, Inst, Globals);
	error ->
	    do_operation(PIDM, OpTab, ["nop"], Globals)
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
		    control(PIDM,Program,OpTab,Globals,Data,PC+1)
	    end
    end,
    maps:get(main,PIDM) ! ok.

do_operation(PIDM, OpTab, Op, Globals) ->
    IsMem = lists:member(hd(Op),dict:fetch_keys(OpTab)),
    if 
	IsMem ->
	    logger:debug("Op: ~p",[Op]),
	    do_op(PIDM,Op,OpTab,Globals),
	    timer:sleep(10),
	    self() ! ok;
	true ->
	    case hd(Op) of
		_Else ->
		    logger:info("Op: is unkown ~p",[_Else])
	    end,
	    %% timer:sleep(10),
	    self() ! ok
    end.
    %% ok.

do_op(PIDM,Op,OpTab,Globals) ->
    logger:info("Op: ~p",[Op]),
    [DR,AL,Pat] = dict:fetch(hd(Op),OpTab),
    {DA,Args} = {hd(lists:sublist(Op,DR,1)),get_arguments(PIDM,Op,AL,Globals)},
    save_to_location(PIDM, DA, do_pat(Pat,Args),Globals).

do_pat(Pat,Args) ->
    logger:debug("Pat: ~p, Args: ~p",[Pat,Args]),
    if 
	(length(Pat) =:= 1) -> 
	    hd(Args);
	true ->
	    case lists:last(Pat) of
		"add" ->
		    hd(Args)+get(get(2,Pat),Args);
		"sub" ->
		    hd(Args)-get(get(2,Pat),Args);
		"mul" ->
		    hd(Args)*get(get(2,Pat),Args);
		"bsl" ->
		    hd(Args) bsl get(get(2,Pat),Args);
		"bsr" ->
		    hd(Args) bsr get(get(2,Pat),Args);
		"addi" ->
		    (hd(Args)+get(get(2,Pat),Args)) rem (1 bsl 12); %% same as %lo(global)
		"lui" -> (hd(Args) bsr 12) bsl 12;                    %% same as %hi(global)
		"sw" -> hd(Args);
		_Default ->
		    error
	    end
    end.

get(N,L) ->
    hd(lists:sublist(L,N,1)).

get_arguments(PIDM,Op,L,Globals) ->
    LL = lists:foldl(fun(X,Acc) -> Acc++lists:sublist(Op,X,1) end, [], L),
    logger:debug("get_arguments: ~p,~p",[Op,L]),
    lists:foldl(fun(A,Acc) ->
			case rvsutils:code_to_object(A) of 
			    {integer,Val,_} ->
				Acc++[Val];
			    {register,Name,_} ->
				logger:debug("get_argument: register ~p",[Name]),
				Val = get_register(PIDM,Name),
				Acc ++ [Val];
			    {memory_access_via_register,Ofs,Name} ->
				logger:debug("get_argument: memory_access_via_register ~p",[Name]),
				Add = get_register(PIDM,Name),
				logger:info("load memory ... ~p",[Add+Ofs]),
				TimeOutLoad = 100,
				maps:get(memory,PIDM) ! {self(),load,Add+Ofs},
				receive
				    {ok,Val} ->
				    	logger:debug("get_argument register: ~p: ~pi",[Name,Val]),
					logger:info("load memory ... ~p: ~p",[Add,Val]),
					Acc++[Val]
				after
				    TimeOutLoad ->
					logger:error("timeout load memory"),
					timeout
				end;
			    {memory_access_via_global_hi,_,G} ->
				logger:debug("get_argument: memory_address_global_hi ~p",[G]),
				{_Prefix,Add} = rvsmemory:derive_address(PIDM,Globals,A),
				Acc++[(Add bsr 4096) bsl 4096];
			    {memory_access_via_global_lo,_,G} ->
				logger:debug("get_argument: memory_access_via_global_hi ~p",[G]),
				{_Prefix,Add} = rvsmemory:derive_address(PIDM,Globals,A),
				logger:info("use address ... ~p",[Add]),
				Acc++[Add rem 4096];
			    _R ->
				logger:error("get_argument failed: ~p",[_R]),
				Acc++[error]
			end
		end, [],LL).
				
get_register(PIDM, Name) ->
    logger:debug("get register: ~p",[Name]),
    maps:get(registers,PIDM) ! {self(),load,Name},
    TimeOutLoad = 100,
    receive
	{ok,Val} ->
	    logger:debug("get register: ~p: ~p",[Name,Val]),
	    Val
    after
	TimeOutLoad ->
	    logger:error("get register failed: ~p: Timeout",[Name]),
	    timeout
    end.

save_to_location(PIDM, DA, Val,Globals) ->
    logger:info("save to location ~p,~p",[DA,Val]),
    Regs = rvsutils:registernames(32),
    IsReg = lists:member(DA,Regs),
    if IsReg ->
	    logger:info("store in register ~p -> ~p",[Val,DA]),
	    maps:get(registers,PIDM) ! {self(), store, DA, Val},
	    TimeOutSave = 100,
	    receive
		ok ->
		    ok
	    after
		TimeOutSave ->
		    io:format("TimeOutSave~n"),
		    timeout
	    end;
       true ->
	    logger:info("try save in memory (reladdr): ~p: ~p",[DA,Val]),
	    case rvsmemory:derive_address(PIDM,Globals,DA) of
		{absval,Add} -> 
		    logger:info("try save in memory (absaddr): ~p: ~p",[Add,Val]),
		    if Val =:= "zero" ->
			    logger:info("!!!!! store Val: zero at ~p",[Add]),
			    maps:get(memory,PIDM) ! {self(), store, Add, 0};
		       true ->
			    logger:info("!!!!! store ~p at ~p",[Val,Add]),
			    maps:get(memory,PIDM) ! {self(), store, Add, Val}
		    end,
		    TimeOutSave = 10,
		    receive
			ok ->
			    ok
		    after
			TimeOutSave ->
			    logger:info("save_to_memory TimeOut",[]),
			    timeout
		    end;
		{"%hi",Val} -> logger:error("%hi not supported yet");
		{"%hi",Val} -> logger:error("%lo not supported yet");
		_R -> logger:error("~p not supported yet",[_R])
	    end
    end.

dump(Registers) ->
    io:format("DumpRegisters: ~p~n",[Registers]),
    Registers.

registers(init,Size,Filling) ->
    registers(lists:foldl(fun(X,Acc) -> 
				  Acc++[{X,Filling}] end,
			  [], rvsutils:registernames(Size))).

registers(Registers) ->
    TimeOut = 2000,
    RMap = maps:from_list(Registers),
    receive
	kill ->
	    io:format("registers killed~n",[]),
	    ok;
	{ PID, dump } ->
	    PID ! {ok,Registers},
	    registers(Registers);
	{ PID, load, Address } ->
	    logger:debug("registers load: ~p~n",[Address]),
	    PID ! {ok,maps:get(Address,RMap)},
	    registers(Registers);
	{ PID, store, Address, Value } ->
	    logger:debug("registers store: ~p: ~p",[Address,Value]),
	    RR=maps:to_list(maps:put(Address,Value,RMap)),
	    PID ! ok,
	    registers(RR)
    after
	TimeOut ->
	    logger:notice("registers timeout",[]),
	    timeout
    end.


-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
store_load_register_test() ->
    PIDReg = spawn(rvscorehw,registers,[init,32,0]),
    PIDReg ! {self(),store,"s0",117},
    TimeOut = 100,
    receive
	ok ->
	    logger:info("store ok"),
	    ?assert(true),
	    ok
    after
	TimeOut ->
	    logger:info("store failed: timeout"),
	    ?assert(false)
    end,
    PIDReg ! {self(),load,"s0"},
    TimeOut = 100,
    receive
	{ok,Val} ->
	    ?assertEqual(117,Val),
	    logger:info("load succes ~p",[Val])
    after
	TimeOut ->
	    logger:info("load timeout",[]),
	    ?assert(false)
    end.
register_timeout_test() ->
    PID=spawn(rvscorehw,registers,[init,32,0]),
    timer:sleep(2100),
    ?assert(is_process_alive(PID)=:=false),
    ok.
do_pat_test() ->
    ?assertEqual(17,do_pat([1],[17])),
    ?assertEqual(20,do_pat([1,2,"addi"],[17,3])),
    ?assert(error=:=rvscorehw:do_pat([1,2,"xxx"],[17,3])).
load_timeout_test() ->
    PID=spawn(fun()->timer:sleep(10) end),
    ?assertEqual([timeout],get_arguments(maps:from_list([{registers,PID}]),["a1"],[1],#{})).
save_timeout_test() ->
    PID=spawn(fun()->timer:sleep(10) end),
    ?assertEqual(timeout,save_to_location(maps:from_list([{registers,PID}]),"a1",[1],#{})).
dump_register_test() ->
    PIDReg = spawn(rvscorehw,registers,[init,32,0]),
    PIDReg ! {self(),dump},
    receive
	{ok,Registers} ->
	    dump(Registers)
    end,
    PIDReg ! kill.
-endif.
