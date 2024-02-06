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
    IsAliveMem = (is_process_alive(maps:get(memory,PIDM))=:=true),
    if IsAliveRegs-> 
	    ?assert(false); 
       true -> 
	    ?assert(true)
    end,
    if IsAliveMem ->
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
    ok.
rvsmain_run_test() ->
    rvsmain_run("",[]).
rvsmain_run_test1_test() ->
    {_,{_,Mem,_}} = rvsmain:run("_build/obj/test1.s",[{dumpmemory,[400,415]}]),
    A=rvsda:getdata(Mem,0,int32),
    B=rvsda:getdata(Mem,4,int32),
    C=rvsda:getdata(Mem,8,int32),
    ?assertEqual(A+B+((A*C) bsr 1),rvsda:getdata(Mem,12,int32)),
    ok.
rvsmain_run_test2_test() ->
    {_,{_,Mem,_}} = rvsmain:run("_build/obj/test2.s",[{input,["2","0","5"]},{dumpmemory,[500,560]}]),
    ?assertEqual(620,rvsda:getdata(Mem,12,uint32)),
    ?assertEqual(471,rvsda:getdata(Mem,52,uint32)).
factorial(Num) -> 
    if
	Num < 1   -> 0;
	Num =:= 1 -> 1;
	true      -> Num * factorial(Num-1)
    end.
f4(I) ->
    case rvsmain:run("_build/obj/test4.s",[{input,[integer_to_list(I)]}]) of
	{_,{_,_,Ret}} -> ?assertEqual(factorial(I),Ret);
	_ -> ?assert(false)
    end.
rvsmain_run_test4fac_1_test() ->
    f4(1).
rvsmain_run_test4fac_2_test() ->
    f4(2).
rvsmain_run_test4fac_3_test() ->
    f4(3).
rvsmain_run_test4fac_4_test() ->
    f4(4).
rvsmain_run_test4fac_5_test() ->
    f4(5).
rvsmain_run_test4fac_6_test() ->
    f4(6).
fibonacci(N) ->
    if 
	N < 0 -> 0;
	N < 2 -> N;
	true -> fibonacci(N-1) + fibonacci(N-2)
    end.
fib4(I) ->
    case rvsmain:run("_build/obj/test4.s",[{input,["0",integer_to_list(I)]}]) of
	{_,{_,_,Ret}} -> ?assertEqual(fibonacci(I),Ret);
	_ -> ?assert(false)
    end.
rvsmain_run_test4fib_1_test() ->
    fib4(1).
rvsmain_run_test4fib_2_test() ->
    fib4(2).
rvsmain_run_test4fib_3_test() ->
    fib4(3).
rvsmain_run_test4fib_4_test() ->
    fib4(4).
rvsmain_run_test4fib_5_test() ->
    fib4(5).
rvsmain_run_test4fib_6_test() ->
    fib4(6).
-endif.
