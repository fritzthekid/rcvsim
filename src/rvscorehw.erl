-module(rvscorehw).
-compile(export_all).

control(PIDM, Program, OpTab, Globals, Data, PC) ->
    Ret = maps:find(PC,maps:from_list(Program)),
    if
	(Ret =:= error) ->
	    do_operation(PIDM, OpTab, ["nop"], Globals);
	true ->
	    {ok,Inst} = Ret,
	    do_operation(PIDM, OpTab, Inst, Globals)
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

do_operation_test(Op) ->
    PIDReg = spawn(rvscorehw,registers,[init,32,0]),
    PIDMem = spawn(rvsmemory,memory,[init,1000,0]),
    PIDM = maps:from_list([{registers,PIDReg},{memory,PIDMem}]),
    {ok, [OpTabL]} = file:consult("src/operation-table.config"),
    OpTab = dict:from_list(OpTabL),
    Globals = maps:from_list([{"buffer",maps:from_list([{address,500}])}]),
    do_operation(PIDM, OpTab, Op, Globals).

do_operation(PIDM, OpTab, Op, Globals) ->
    IsMem = lists:member(hd(Op),dict:fetch_keys(OpTab)),
    if 
	IsMem ->
	    %logger:info("Op: ~p",[Op]),
	    do_op(PIDM,Op,OpTab,Globals),
	    timer:sleep(10),
	    self() ! ok;
	true ->
	    case hd(Op) of
		%% "lw" ->
		%%     ok; %io:format("Op: is lw~n",[]);
		%% "sw" ->
		%%     ok; %io:format("Op: is sw~n",[]);
		%% "mv" ->
		%%     ok; %io:format("Op: is mv~n",[]);
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
    save_to_register(PIDM, DA, do_pat(Pat,Args),Globals).

do_pat(Pat,Args) ->
    logger:debug("Pat: ~p, Args: ~p",[Pat,Args]),
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
		    (hd(Args)+get(get(2,Pat),Args)) rem (1 bsl 12); %% same as %lo(global)
		lui -> (hd(Args) bsr 12) bsl 12;                    %% same as %hi(global)
		_Default ->
		    error
	    end
    end.

get(N,L) ->
    hd(lists:sublist(L,N,1)).

get_arguments(PIDM,Op,L,Globals) ->
    LL = lists:foldl(fun(X,Acc) -> Acc++lists:sublist(Op,X,1) end, [], L),
    %% Regs = lists:foldl(fun(X,Acc)->Acc++["a"++integer_to_list(X)] end,[],
    %% lists:seq(0,64))++["sp","s0"],
    logger:debug("get_arguments: ~p,~p",[Op,L]),
    lists:foldl(fun(A,Acc) ->
			case rvsutils:code_to_object(A) of 
			    {integer,Val,_} ->
				Acc++[Val];
			    {register,Name,_} ->
				logger:debug("get_argument: register ~p,~p",[A,Name]),
				TimeOutLoad=100,
				maps:get(registers,PIDM) ! {self(),load,Name},
				receive
				    {ok,Val} ->
					logger:debug("get_argument ~p <- ~p",[Name,Val]),
					Acc++[Val];
				    _ ->
					Acc++[error]
				after
				    TimeOutLoad ->
					io:format("TimeOutLoad~n",[]),
					timeout
				end;
			    {memory_access_via_register,Ofs,Name} ->
				logger:debug("get_argument: memory_access_via_register ~p",[Name]),
				Add = get_register(PIDM,Name),
				logger:info("load memory ... ~p",[Add+Ofs]),
				TimeOutLoad = 100,
				maps:get(memory,PIDM) ! {self(),load,Add+Ofs},
				receive
				    {ok,Val} ->
					logger:debug("get_argument register: ~p: ~pi",[Name,Val]),
					logger:info("load memory ... ~p",[Add]),
					TimeOutLoad = 100,
					maps:get(memory,PIDM) ! {self(),load,Val},
					receive
					    {ok,LVal} ->
						logger:debug("get_argument memory: ~p: ~pi",[Add,LVal]),
						Acc++[LVal];
					    _ ->
						Acc++[error]
					end;
				    _ ->
					logger:warning("load memory ... ~p",[Add]),
					Acc++[error]
				end;
			    {memory_access_via_global_hi,_,G} ->
				logger:debug("get_argument: memory_access_via_global_hi ~p",[G]),
				{_Prefix,Add} = rvsmemory:derive_address(PIDM,Globals,A),
				logger:info("load memory ... ~p",[Add]),
				maps:get(memory,PIDM) ! {self(),load,Add},
				TimeOutLoad = 100,
				receive
				    {ok,Val} ->
					logger:debug("get_argument %hi(~p): ~p",[Add,Val]),
					Acc++[(Val bsr 4096) bsl 4096];
				    _ ->
					Acc++[error]
				after
				    TimeOutLoad ->
					io:format("TimeOutLoad~n",[]),
					timeout
				end;
			    {memory_access_via_global_lo,_,G} ->
				logger:debug("get_argument: memory_access_via_global_hi ~p",[G]),
				{_Prefix,Add} = rvsmemory:derive_address(PIDM,Globals,A),
				logger:info("load memory ... ~p",[Add]),
				TimeOutLoad = 100,
				maps:get(memory,PIDM) ! {self(),load,Add},
				receive
				    {ok,Val} ->
					logger:debug("get_argument %hi(~p): ~",[Add,Val]),
					%% "%lo" ->
					Acc++[Val rem 4096];
				    _ ->
					Acc++[error]
				after
				    TimeOutLoad ->
					io:format("TimeOutLoad~n",[]),
					timeout
				end;
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

%% if
%%     is_number(A) ->
%%         logger:debug("get_argument: is_number ~p,~p",[A,Acc]),
%% 	Acc++[A];
%%     A =:= "zero" ->
%% 	logger:debug("get_argument: is_zero ~p,~p",[A,Acc]),
%% 	Acc++[0];
%%     is_list(A) ->
%% 	logger:debug("get_argument: is_list ~p",[A]),
%% 	TimeOutLoad=100,
%% 	IsReg = lists:member(A,Regs),
%% 	if IsReg ->
%% 		maps:get(registers,PIDM) ! {self(),load,A},
%% 		receive
%% 		    {ok,Val} ->
%% 			Acc++[Val];
%% 		    _ ->
%% 			Acc++[error]
%% 		after
%% 		    TimeOutLoad ->
%% 			logger:notice("TimeOut load argument"),
%% 			timeout
%% 		end;
%% 	   true ->
%% 		logger:debug("try load memory ... ~s",[A]),
%% 		{Prefix,Add} = rvsmemory:derive_address(PIDM,Globals,A),
%% 		logger:info("load memory ... ~p",[Add]),
%% 		maps:get(memory,PIDM) ! {self(),load,Add},
%% 		receive
%% 		    {ok,Val} ->
%% 			case Prefix of
%% 			    absval ->
%% 				Acc++[Val];
%% 			    "%hi" ->
%% 				Acc++[(Val bsr 4096) bsl 4096];
%% 			    "%lo" ->
%% 				Acc++[Val rem 4096]
%% 			end;
%% 		    _ ->
%% 			Acc++[error]
%% 		after
%% 		    TimeOutLoad ->
%% 			io:format("TimeOutLoad~n",[]),
%% 			timeout
%% 		end
%% 	end
%% end

save_to_register(PIDM, DA, Val,Globals) ->
    Regs = lists:foldl(fun(X,Acc)->Acc++["a"++integer_to_list(X)] end,[],
		       lists:seq(0,64))++["sp","s0"],
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
				  Acc++[{"a"++integer_to_list(X),Filling}] end,
			  [], lists:seq(0,Size))++[{"sp",0},{"s0",0}]).

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
    timer:sleep(1100),
    ?assert(is_process_alive(PID)=:=false),
    ok.
do_pat_test() ->
    ?assertEqual(17,do_pat([1],[17])),
    ?assertEqual(20,do_pat([1,2,addi],[17,3])),
    ?assert(error=:=rvscorehw:do_pat([1,2,xxx],[17,3])).
load_timeout_test() ->
    PID=spawn(fun()->timer:sleep(10) end),
    ?assertEqual(timeout,get_arguments(maps:from_list([{registers,PID}]),["a1"],[1],#{})).
save_timeout_test() ->
    PID=spawn(fun()->timer:sleep(10) end),
    ?assertEqual(timeout,save_to_register(maps:from_list([{registers,PID}]),"a1",[1],#{})).
dump_register_test() ->
    PIDReg = spawn(rvscorehw,registers,[init,32,0]),
    PIDReg ! {self(),dump},
    receive
	{ok,Registers} ->
	    dump(Registers)
    end,
    PIDReg ! kill.
-endif.
