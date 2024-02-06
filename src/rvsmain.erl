-module(rvsmain).
-compile([export_all]).

kill(PIDL) ->
    lists:foreach(fun(X) -> exit(X,kill) end, PIDL).

run() ->
    run("_build/obj/simple-func.s",[]).
run(Filename) ->
    run(Filename,[]).
run(Filename,ConfigList) ->
    ROOT="./",
    {ok, [RawConfig]}  = file:consult("data/rvs.config"),
    Config = lists:foldl(fun({X,Y},Map) -> maps:put(X,Y,Map) 
			 end, RawConfig,ConfigList++[{programname,Filename}]),
    {PP,Defines} = rvsreadasm:readasm(maps:get(programname,Config)),
    rvsutils:write_terms("_build/tmp/program.s",[PP]),
    rvsutils:write_terms("_build/tmp/globals_labels.config",[Defines]),
    {ok, [Data]} = file:consult(ROOT ++ "/data/" ++ maps:get(dataname,Config)),
    PIDRegs = spawn(rvscorehw, registers, [init,maps:get(registers,Config),0]),
    PIDMem =  spawn(rvsmemory, memory, [init,maps:get(memory,Config),100]),
    PIDM = maps:from_list([{registers,PIDRegs},{memory,PIDMem},{main,self()}]),
    case maps:find(input,Config) of
	{ok,Values} -> update_sysargs(PIDM,Values);
	error -> ok
    end,
    PIDCtrl = spawn(rvscorehw, control, [PIDM, PP, Defines, Data, 0]),
    TimeOutMain = maps:get(timeout_main,Config),
    receive
	ok ->
	    logger:debug("main got ok - and finishes regularily"),
	    ok
    after
	TimeOutMain ->
	    logger:notice("timeout main"),
	    timeout
    end,
    timer:sleep(1000),
    Output = output_config(PIDM,Config),
    logger:info("Config: ~p",[Config]),
    timer:sleep(100),
    exit(PIDRegs,kill),exit(PIDMem,kill),
    case is_process_alive(PIDCtrl) of
	true -> exit(PIDCtrl,kill);
	_ -> ok
    end,
    {{maps:put(main,self(),PIDM),PIDCtrl},Output}.

program_to_strings(Program) ->
    lists:foldl(fun(Op, Acc) -> Acc ++ [op_to_string(Op)] end, [], Program).

op_to_string(Operation) ->
    list_to_tuple(lists:foldl(fun(X,Acc)->
				      if 
					  (is_number(X) or is_list(X)) ->
					      Acc++[X]; 
					  is_atom(X) ->
					      Acc++[atom_to_list(X)];
					  true ->
					      Acc
				      end
			      end, [], tuple_to_list(Operation))).
							     
update_sysargs(PIDM,Values) ->
    if 
	is_list(Values) ->
	    rvscorehw:save_memory(PIDM,4000,length(Values)),
	    F = fun(X,{I,LI}) -> 
			case is_list(X) of
			    true ->
				rvscorehw:save_memory(PIDM,4004+I,LI),
				rvslibs:save_string(PIDM,X,LI),
				{I+4,LI+length(X)+1};
			    _ ->
				rvscorehw:save_memory(PIDM,4004+I,X),
				{I+4,LI}
			end
		end,
	    lists:foldl(F,{0,4100},Values);
	true ->
	    throw({"input values not an array, usage {input,[1,2,3]}, but",Values})
    end.

output_config(PIDM, Config) ->
    Regs = case maps:find(dumpregs, Config) of
	       {ok, _Range} ->
		   dump_registers(PIDM);
	       _ ->
		   []
	   end,
    Mem = case maps:find(dumpmemory, Config) of
	      {ok,[A,B]} ->
		  dump_memory(PIDM,A,B);
	      {ok,_R} ->
		  throw({"Usage {dumpmemory,[A,B]}: got dumpmemory,",_R});
	      _ ->
		  []
	  end,
    RetVal = rvscorehw:load_register(PIDM,"a0"),
    {Regs,Mem,RetVal}.

dump_registers(PIDM) ->
    maps:get(registers,PIDM) ! { self(), dump },
    TimeOutDump = 100,
    receive
	{ok,Registers} ->
	    Registers,
	    lists:sort(fun({A,_},{B,_}) -> A < B end, Registers)
    after
	TimeOutDump ->
	    logger:error("timeout dump_registers"),
	    timeout
    end.

dump_memory(PIDM, A, E) ->
    maps:get(memory,PIDM) ! { self(), dump },
    TimeOutDump = 100,
    Memory = receive
		 {ok,Mem} ->
		     Mem
	     after
		 TimeOutDump ->
		     logger:error("timeout dump_memory"),
		     timeout
	     end,
    case Memory of
	timeout -> timeout;
	_ -> 
	    LMem = lists:foldl(fun(V,Acc)->
				Acc ++ [binary:at(Memory,V)]
			end, [], lists:seq(A,E)),
	    binary:list_to_bin(LMem)
    end.

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
rvsmain_dump_register_and_timeout_dump_memory_test() ->
    PIDRegs = spawn(rvscorehw, registers, [init,32,17]),
    Regs = dump_registers(maps:from_list([{registers,PIDRegs}])),
    ?assertEqual(17,maps:get("a15",maps:from_list(Regs))),
    kill([PIDRegs]),
    ?assertEqual(timeout,dump_registers(maps:from_list([{registers,PIDRegs}]))),
    ?assertEqual(timeout,dump_memory(maps:from_list([{memory,PIDRegs}]),400,404)).
rvsmain_kill_control_test() ->
    {_,_} = run(),
    timer:sleep(1000),
    %%kill(PIDCtrl).	
    ok.
rvsmain_op_to_string_test() ->
    Result = op_to_string({"saf",asd,1,digraph:new()}),
    ?assertEqual(3,tuple_size(Result)),
    ok.
-endif.
