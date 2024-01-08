-module(rvsmain).
-compile([export_all]).

kill(PIDL) ->
    lists:foreach(fun(X) -> X ! kill end, PIDL).

run() ->
    run("test/assembler/simple-func.s",[]).
run(Filename) ->
    run(Filename,[]).
run(Filename,ConfigList) ->
    ROOT="./",
    {ok, [RawConfig]}  = file:consult("data/rvs.config"),
    Config = lists:foldl(fun({X,Y},Map) -> maps:put(X,Y,Map) 
			 end, RawConfig,ConfigList++[{programname,Filename}]),
    rvsutils:write_terms("bck/config.cfg",[Config]),
    {[_|P],Globals} = rvsreadasm:readasm(maps:get(programname,Config)),
    PP =  element(2,lists:foldl(fun(X,{I,Acc}) -> 
				       {I+1, Acc++[{I,tuple_to_list(X)}]} 
			       end, {0, []}, program_to_strings(P))),
    rvsutils:write_terms("bck/program.s",[PP]),
    {ok, [Data]} = file:consult(ROOT ++ "/data/" ++ maps:get(dataname,Config)),
    {ok, [OTL]} = file:consult(ROOT ++ "/src/operation-table.config"),
    OpTab = dict:from_list(OTL),
    PIDRegs = spawn(rvscorehw, registers, [init,maps:get(registers,Config),0]),
    PIDMem =  spawn(rvsmemory, memory, [init,maps:get(memory,Config),100]),
    PIDM = maps:from_list([{registers,PIDRegs},{memory,PIDMem},{main,self()}]),
    PIDCtrl = spawn(rvscorehw, control, [PIDM, PP, OpTab, Globals, Data, 0]),
    %%maps:fold(fun(_,V,Acc)->[V]++Acc end,[],PIDM)++[PIDCtrl].
    TimeOutMain = maps:get(timeout_main,Config),
    receive
	ok ->
	    ok
    after
	TimeOutMain ->
	    timeout
    end,
    timer:sleep(1000),
    Regs = do_dump_config(PIDM,Config),
    logger:info("Config: ~p",[Config]),
    timer:sleep(100),
    kill(maps:fold(fun(_,V,Acc) -> Acc++[V] end, [], PIDM)),
    SRegs = if is_list(Regs) ->
		    lists:sort(fun({A,_},{B,_}) -> A < B end, Regs);
	       true->
		    Regs
	    end,
    {{maps:put(main,self(),PIDM),PIDCtrl},SRegs}.
    
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
							     
do_dump_config(PIDM, Config) ->
    case maps:find("dump", Config) of
	{ok, "registers"} ->
	    dump_registers(PIDM);
	{ok, {"memory",[A,B]}} ->
	    logger:info("dump memory: ~p - ~p",[A,B]),
	    dump_memory(PIDM,A,B);
	_R ->
	    []
    end.

dump_registers(PIDM) ->
    maps:get(registers,PIDM) ! { self(), dump },
    TimeOutDump = 100,
    receive
	{ok,Registers} ->
	    Registers
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
    lists:foldl(fun(V,Acc)->
			Acc ++ [{V,array:get(V,Memory)}]
		  end, [], lists:seq(A,E)).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
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
