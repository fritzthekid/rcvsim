-module(rvscorehw).
-compile(export_all).

control(PIDM, Program, Defines, Data, PC) ->
    case maps:find(PC,maps:from_list(Program)) of
	{ok, Inst} ->
	    case do_operation(PIDM, Inst, Defines, PC) of
		ok ->
		    if 
			PC >= length(Program) ->
			    maps:get(main,PIDM) ! ok;
			true ->
			    control(PIDM,Program,Defines,Data,PC+1)
		    end;
		{ok,jump,Target} ->
		    if
			Target < 0 ->
			    control(PIDM,Program,Defines,Data,PC+1);
			true ->
			    control(PIDM,Program,Defines,Data,Target)
		    end;
		{ok,exit} ->
		    maps:get(main,PIDM) ! ok;
		_Default ->
		    throw({"control stops with strange end:",_Default})		    
		end;
	error ->
	    ok = do_operation(PIDM, ["nop"], Defines, PC),
	    if 
		PC >= length(Program) ->
		    maps:get(main,PIDM) ! ok;
		true ->
		    control(PIDM,Program,Defines,Data,PC+1)
	    end
    end.

do_operation(_,["nop"],_,_) ->
    logger:info("do_operation: nop, do nothing"),
    ok;
do_operation(PIDM, Op, Defines,PC) ->
    logger:info("do_operation: Op ~p",[Op]),
    {Globals,Labels} = Defines,
    case hd(Op) of
	"exit" -> 
	    logger:notice("exit normally"),
	    { ok, exit };
	"ret" ->
	    logger:info("rvscorehw,do_operation: return"),
	    do_return(PIDM,Defines,PC);
	"jr" -> %% jump register
	    Targets = get_arguments(PIDM,[lists:last(Op)],Globals),
	    logger:info("rvscorehw do_operation: jump target ~p ~p",[hd(Op),hd(Targets)]),
	    %%Target = lists:last(Op),
	    {ok,jump,hd(Targets)}; %%maps:get(Target,Labels,-1)};
	"j" -> %% jump label
	    %%Target = get_arguments(PIDM,[lists:last(Op)],Globals),
	    Target = lists:last(Op),
	    logger:info("rvscorehw do_operation: jump target ~p num ~p",[Target,maps:get(Target,Labels,"not_found")]),
	    {ok,jump,maps:get(Target,Labels,-1)};
	"call" ->
	    ok = save_register(PIDM,"ra",PC+1),
	    Target = maps:get(lists:last(Op),Labels),
	    {ok,jump,Target};
	"sw" ->
	    [_|[Arg|_]] = Op,
	    do_op(PIDM,["sw"],lists:last(Op),calcop,[Arg],Defines);
	_ ->
	    case opstype(Op) of
		{nop,nop,nop} ->
		    logger:notice("rvscorehw,do_operation: Op ~p unknown",[Op]),
		    ok;
		{DA,Arguments,OpType} ->
		    do_op(PIDM,Op,DA,OpType,Arguments,Defines);
		_Default ->
		    logger:error("do_operation got strange optype: ~p",[_Default]),
		    throw({"do_operation got strange optype:",_Default})
	    end
    end.

opstype(Op) ->
    IsMCalOp=lists:member(hd(Op),["addi","add","sud","mul","rem","slli","srai",
				  "load","lui","li","lw","mw","mv"]),
    IsMBraOp=lists:member(hd(Op),["bne","beqz","bnez","bltz","bgtz","bgt","ble","bge","bgtu"
				  "bleu","bleu"]),
    case {IsMCalOp,IsMBraOp} of
	{true,false} ->
	    [_|[H|T]] = Op,
	    {H,T,calcop};
	{false,true} ->
	    [_|Rest] = Op,
	    [H|T] = lists:reverse(Rest),
	    {H,lists:reverse(T),branchop};
	_ ->
	    {nop,nop,nop}
    end.		     

do_return(PIDM,Defines,PC) ->
    logger:notice("do_return"),
    do_operation(PIDM,["jr","ra"],Defines,PC).

do_op(_,["nop"],_,_,_,_) ->
    logger:notice("rvscorehw:do_op(nop)"),
    ok;
do_op(PIDM,Op,DA,OpType,ArgsList,{Globals,Labels}) ->
    logger:debug("do_op: Op ~p",[Op]),
    Args = get_arguments(PIDM,ArgsList,Globals),
    PatResult = do_pat(Op,Args),
    logger:debug("do_op: Op ~p, PatResult ~p, ArgsList ~p, Args ~p",[Op,PatResult,ArgsList,Args]),
    case {PatResult,OpType} of
	{error,_} ->
	    logger:notice("do_op: operation ~p unknown, do nop",[Op]);
	{_,calcop} -> 
	    save_to_location(PIDM, DA, PatResult,Globals),
            ok;
	{_,branchop} ->
	    case maps:is_key(lists:last(Op),Labels) of
		true ->
		    JumpTo = maps:get(lists:last(Op),Labels),
		    logger:debug("do_op jump to ~p, ~p",[JumpTo]),
		    case PatResult of
			true->
			    {ok,jump,JumpTo};
			false ->
			    {ok,jump,-1};
			R ->
			    throw({"PatResult, neither true nor false:",R})
		    end;
		R ->
		    logger:error("do_op,branchop: ~p is not a target ~p",[DA,R]),
		    throw({"branch target is not a label",DA,Args,ArgsList,lists:last(Op)})
	    end			
    end.

do_pat(Op,Args) ->
    logger:debug("HOp ~p, Arg: ~p",[hd(Op),Args]),
    case hd(Op) of
	"add" -> hd(Args)+get(2,Args);
	"sub" -> hd(Args)-get(2,Args);
	"mul" -> hd(Args)*get(2,Args);
	"rem" -> hd(Args) rem get(2,Args);
	"bsl" -> hd(Args) bsl get(2,Args);
	"bsr" -> hd(Args) bsr get(2,Args);
	"addi" -> (hd(Args)+get(2,Args)) rem (1 bsl 12); %% same as %lo(global)
	"lui" -> (hd(Args) bsr 12) bsl 12;                    %% same as %hi(global)
	"li" -> hd(Args);
	"load" -> hd(Args);
	"slli" -> hd(Args) bsl get(2,Args);
	"srai" -> hd(Args) bsr get(2,Args);
	"lw" -> hd(Args);
	"mv" -> hd(Args);
	"mw" -> hd(Args);
	"sw" -> hd(Args);
	"beqz" -> hd(Args) =:= 0;
	"bnez" -> hd(Args) =/= 0;
	"blez" -> hd(Args) >= 0;
	"bltz" -> hd(Args) < 0;
	"bgtz" -> hd(Args) > 0;
	"bne" -> hd(Args) =/= get(2,Args);
	"bge" -> hd(Args) >= get(2,Args);
	"bgt" -> hd(Args) > get(2,Args);
	"ble" -> hd(Args) =< get(2,Args);
	"blt" -> hd(Args) < get(2,Args);
	%%"bgtu" -> hd(Args) =< get(2,Args);
	%%"bleu" -> hd(Args) =< get(2,Args);
	_Default ->
	    error
    end.

get(N,L) ->
    hd(lists:sublist(L,N,1)).

get_arguments(PIDM,LL,Globals) ->
    logger:debug("get_arguments: ~p",[LL]),
    F = fun(A,Acc) ->
		case rvsutils:code_to_object(A) of 
		    {integer,Val,_} ->
			Acc++[Val];
		    {register,Name,_} ->
			logger:debug("get_argument: register ~p",[Name]),
			Val = load_register(PIDM,Name),
			Acc ++ [Val];
		    {memory_access_via_register,Ofs,Name} ->
			Add = load_register(PIDM,Name),
			Val = load_memory(PIDM,Add+Ofs),
			Acc++[Val];
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
	end,
    lists:foldl(F, [],LL).

save_to_location(PIDM, DA, Val,Globals) ->
    logger:info("save to location ~p,~p",[DA,Val]),
    Regs = rvsutils:registernames(32),
    case lists:member(DA,Regs) of
	true ->
	    logger:debug("save_to_location, ismember of registers: ~p,~p",[DA,Val]),
	    save_register(PIDM,DA,Val);
	_ ->
	    logger:info("try save in memory (reladdr): ~p: ~p",[DA,Val]),
	    case rvsmemory:derive_address(PIDM,Globals,DA) of
		{absval,Add} -> 
		    logger:info("try save in memory (absaddr): ~p: ~p",[Add,Val]),
		    if Val =:= "zero" ->
			    logger:info("!!!!! store Val: zero at ~p",[Add]),
			    save_memory(PIDM,Add,0);
		       true ->
			    logger:info("!!!!! store ~p at ~p",[Val,Add]),
			    save_memory(PIDM,Add, Val)
		    end;
		{"%lo",TVal} -> 
		    logger:notice("rvscorehw,save_to_location: %lo(~p)(Reg) is shorthand: ~p",
				  [TVal,DA]),
		    Addd = setup_value(PIDM,DA,TVal),
		    save_memory(PIDM,Addd,Val);
		_R -> logger:error("rvscorehw,save_to_location: ~p not supported yet",[_R])
	    end
    end.

setup_value(PIDM,DA,Val) ->
    logger:info("setup_register ~p ~p",[DA,Val]),
    L = lists:foldl(fun(W,Acc)->Acc++string:split(W,")") end, [], string:split(DA,"(",all)),
    Reg = 
	case length(L) of
	    5 ->
		[_,_,_,R,_] = L,
		R;
	    _ ->
		logger:error("setup_register fails ~p not short hand",[DA]),
		error
	end,
    maps:get(registers,PIDM) ! { self(), load, Reg },
    TimeOut = 100,
    V = receive
	    {ok, VV} ->
		VV
	after
	    TimeOut -> timeout
	end,
    Val+V.

save_register(PIDM,Reg,Val)->
    logger:info("rvscorehw,save_register: ~p -> ~p",[Val, Reg]),
    maps:get(registers,PIDM) ! {self(),store,Reg,Val},
    TimeOut = 100,
    receive
	ok -> 
	    %%logger:debug("rvscorehw,save_register, store passed"),
	    ok
    after
	TimeOut ->
	    logger:error("rvscorehw,save_register ~p, ~p",[Reg,Val]),
	    throw({"save_register Reg,Val timeout",Reg,Val})
    end.

load_register(PIDM,Reg)->
    maps:get(registers,PIDM) ! {self(),load,Reg},
    TimeOut = 100,
    receive
	{ok, Val} -> Val
    after
	TimeOut ->
	    logger:error("rvscorehw,load_register ~p",[Reg]),
	    throw({"load_register Reg timeout",Reg})
    end.

save_memory(PIDM,Add,Val)->
    logger:info("rvscorehw,save_memory: ~p -> ~p",[Val,Add]),
    maps:get(memory,PIDM) ! {self(),store,Add,Val},
    TimeOut = 100,
    receive
	ok -> ok;
	_R -> throw({"save memory strange",_R})
    after
	TimeOut ->
	    logger:error("rvscorehw,save_memory ~p, ~p timeout",[Add,Val]),
	    throw({"save_memory Add,Val timeout",Add,Val})
    end.

load_memory(PIDM,Add)->
    maps:get(memory,PIDM) ! {self(),load,Add},
    TimeOut = 100,
    receive
	{ok, Val} -> Val
    after
	TimeOut ->
	    logger:error("rvscorehw,load_memory, ~p",[Add]),
	    throw({"load_memory Add timeout",Add})
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
	    logger:debug("registers,store: ~p: ~p",[Address,Value]),
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
    TimeOut = 150,
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
    TimeOut = 150,
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
    ?assertEqual(70,do_pat(["mul","sf","sdf"],[10,7])),
    ?assertEqual(17,do_pat(["addi","sf","sdf"],[10,7])),
    ?assertEqual(true,do_pat(["bge","sf","sdf"],[10,7])),
    ?assertException(error,badarith,do_pat(["rem","asdfc","sfd"],[17,0])).
save_timeout_test() ->
    PID=spawn(fun()->timer:sleep(10) end),
    timer:sleep(15),
    ?assertException(_,_,save_register(maps:from_list([{registers,PID}]),"a1",4715)),
    ok.
dump_register_test() ->
    PIDReg = spawn(rvscorehw,registers,[init,32,0]),
    PIDReg ! {self(),dump},
    receive
	{ok,Registers} ->
	    dump(Registers)
    end,
    PIDReg ! kill.
save_and_load_register_test() ->
    PIDReg = spawn(rvscorehw,registers,[init,32,0]),
    PIDM = #{registers => PIDReg},
    save_register(PIDM,"sp",4711),
    Val = load_register(PIDM,"sp"),
    ?assertEqual(4711,Val),
    PIDReg ! kill.
load_and_save_memory_test() ->
    PIDMem = spawn(rvsmemory,memory,[init,100,0]),
    true = is_process_alive(PIDMem),
    PIDM = #{memory => PIDMem},
    true = is_process_alive(maps:get(memory,PIDM)),
    save_memory(PIDM,17,4711),
    Val = load_memory(PIDM,17),
    ?assertEqual(4711,Val),
    exit(PIDMem,kill).
opstypetest_test() ->
    ?assertEqual({"a1",[27],calcop},opstype(["lui","a1",27])),
    ?assertEqual({"a1",["a2","a3"],calcop},opstype(["mul","a1","a2","a3"])),
    ?assertEqual({".L3",["a1"],branchop},opstype(["bnez","a1",".L3"])),
    ?assertEqual({".L2",["a1","a2"],branchop},opstype(["bge","a1","a2",".L2"])),
    ?assertEqual({nop,nop,nop},opstype(["fritz","a1","a2",".L2"])),
    PIDReg = spawn(rvscorehw,registers,[init,32,0]),
    PIDMem = spawn(rvsmemory,memory,[init,4000,0]),
    PIDM = maps:from_list([{registers,PIDReg},{memory,PIDMem}]),
    save_register(PIDM,"a20",17),
    Globals = #{"buffer" => #{address => 400}},
    ok = do_op(PIDM,["li","a20","57"],"a20",calcop,[57],{Globals,#{".L3" => 57}}),
    ?assertEqual(57,load_register(PIDM,"a20")),
    save_memory(PIDM,400,123),
    ?assertEqual(123,load_memory(PIDM,400)),
    ok = do_op(PIDM,["addi","a21","a21","%lo(buffer)"],"a21",calcop,["a21","%lo(buffer)"],{Globals,#{".L3" => 57}}),
    Valaddi = load_register(PIDM,"a21"),
    ?assertEqual(400,Valaddi),
    ok = do_op(PIDM,["lw","a22","0(a21)"],"a22",calcop,["0(a21)"],{Globals,#{".L3" => 57}}),
    Vallw = load_register(PIDM,"a22"),
    ?assertEqual(123,Vallw),
    {ok,jump,Target}=
	do_op(PIDM,["bltz","a20",".L3"],"a1",branchop,["a20"],{Globals,#{".L3" => 57}}),
    ?assertEqual(-1,Target),
    exit(PIDReg,kill),exit(PIDMem,kill).
get_arguments_test() ->
    PIDReg = spawn(rvscorehw,registers,[init,32,0]),
    PIDMem = spawn(rvsmemory,memory,[init,4000,0]),
    PIDM = maps:from_list([{registers,PIDReg},{memory,PIDMem}]),
    save_memory(PIDM,400,127),
    Globals = #{"buffer" => #{address => 400}},
    save_register(PIDM,"a21",400),
    ?assertEqual(400,load_register(PIDM,"a21")),
    Args = get_arguments(PIDM,["0(a21)","%lo(buffer+12)"],Globals),
    ?assertEqual([127,412],Args),
    rvsmain:kill([PIDReg,PIDMem]),
    ok.
control_failure_test() ->
    PID = spawn(fun() -> ok end),
    control(#{main => PID},[],{},[],1),
    control(#{main => PID},[{0,["nop"]},{2,["nop"]}],{},[],1),
    ok.
do_op_failures_test() ->
    ok = do_op(#{},["fritz","7","4"],"4",calcop,["7"],{#{},{}}),
    ?assertException(throw,_,do_op(#{},["bge","7","3",".L3"],".L3",branchop,["7","3"],{#{},#{}})),
    ok = do_op(#{},["nop"],"nop",calcop,[],{}).
do_operation_dummy_test()->
    ok = do_operation(#{},["fritz"],{#{},{}},0).
do_save_to_location_test()->
    ok=save_to_location(#{},"fi%(fritz)",0,#{"fritz" => #{address => 7}}).
-endif.
