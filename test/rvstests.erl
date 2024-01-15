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
    rvsmain_run("_build/obj/func-with-globals.s",[]).
rvsmain_run_add_store_test() ->
    {_,Mem} = rvsmain:run("_build/obj/load-add-store.s",[{"dump", {"memory",[400,411]}}]),
    A=maps:get(400,maps:from_list(Mem)),
    B=maps:get(404,maps:from_list(Mem)),
    ?assertEqual(A+B,maps:get(408,maps:from_list(Mem))),
    ok.
rvsmain_run_test1_test() ->
    {_,Mem} = rvsmain:run("_build/obj/test1.s",[{"dump", {"memory",[400,415]}}]),
    A=maps:get(400,maps:from_list(Mem)),
    B=maps:get(404,maps:from_list(Mem)),
    C=maps:get(408,maps:from_list(Mem)),
    ?assertEqual(A+B+((A*C) bsr 1),maps:get(412,maps:from_list(Mem))),
    ok.
rvsmain_run_test2_test() ->
    {_,Mem} = rvsmain:run("_build/obj/test2.s",[{"dump", {"memory",[400,460]}}]),
    ?assertEqual(620,maps:get(412,maps:from_list(Mem))),
    ?assertEqual(471,maps:get(452,maps:from_list(Mem))).
rvsmain_run_test3_test() ->
    {_,Mem} = rvsmain:run("_build/obj/test3.s",[{"dump", {"memory",[400,460]}}]),
    ?assertEqual(243,maps:get(412,maps:from_list(Mem))).
-endif.
