-module(rvstests).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
rcvmain_do_test() ->
    {PIDM,_PIDCtrl} = rvsmain:do(),
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
-endif.
