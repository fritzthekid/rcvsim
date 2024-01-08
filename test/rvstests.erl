-module(rvstests).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
rvsmain_run(Filename,ConfigList) ->
    {{PIDM,_PIDCtrl},_Regs} = if 
			  Filename =:= "" ->
			      rvsmain:run();
			  true ->
			      rvsmain:run(Filename,ConfigList)
		      end,
    timer:sleep(1000),
    IsAliveRegs = is_process_alive(maps:get(registers,PIDM)),
    IsAlieMem = (is_process_alive(maps:get(memory,PIDM))=:=true),
    if IsAliveRegs-> 
	    ?assert(false); 
       true -> 
	    ?assert(true)
    end,
    if IsAlieMem ->
	    ?assert(false); %%true; 
       true -> 
	    ?assert(true) %%false 
    end,
    _IsAliveCtrl = is_process_alive(_PIDCtrl),
    if _IsAliveCtrl ->
	    true; 
       true -> 
	    false 
    end,
    if _IsAliveCtrl ->
	    rvsmain:kill([_PIDCtrl]);
       true -> ok
    end,
    %%?assert(IsAliveRegs=:=false),
    %%?assert(isAliveMem=:=false),
    %%ssert(is_process_alive(maps:get(memory,PIDM))),
    %%ssert(is_process_alive(PIDCtrl)),
    %%lists:foreach(fun(PID)->?assert((is_process_alive(PID)=:=false)) end, PIDL).
    ok.
rvsmain_run_test() ->
    rvsmain_run("",[]).
rvsmain_run_global_test()->
    rvsmain_run("test/assembler/func-with-globals.s",[]).
%%rvsmain_run_simple_test()->
%%    rvsmain_run("test/assembler/empty.s",[{"dump","registers"}]).
-endif.
