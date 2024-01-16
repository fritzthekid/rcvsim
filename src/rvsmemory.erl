-module(rvsmemory).
-compile(export_all).

memory(init,Size,Filling) ->
    memory(array:new(Size,{default,Filling})).

memory(Memory) ->
    TimeOut = 2000,
    receive
	kill ->
	    io:format("memory killed~n",[]),
	    ok;
	{ PID, dump } ->
	    PID ! {ok,Memory},
	    memory(Memory);
	{ PID, load, Address } ->
	    logger:debug("memory load address ~p",[Address]),
	    if 
		Address < 0 ->
		    logger:warning("load memory at address ~p",[Address]),
		    PID ! {ok, 0};
		true ->
		    Size = array:size(Memory),
		    if Address < Size ->
			    logger:debug("memory load: address ~p: (get value ~p)",[Address,array:get(Address,Memory)]),
			    PID ! {ok,array:get(max(0,Address),Memory)};
		       true -> 
			    logger:error("memory load: address ~p > ~p)",[Address,Size]),
			    PID ! {ok,0}
		    end
	    end,
	    memory(Memory);
	{ PID, store, Address, Value } ->
	    {Lower,Higher} = { (Address<0),(Address>=array:size(Memory)) },
	    logger:info("rvsmemory:memory access is (< 0): ~p or (>=Size): ~p",[Lower,Higher]),
	    case {Lower,Higher} of
		{true,_} ->
		    logger:error("memory access out of range < 0: ~p",[Address]),
		    throw({"memory access out of range",Address});
		{_,true} ->
		    logger:error("memory access out of range >= Size: ~p",[Address]),
		    throw({"memory access out of range",Address});
		{_,_} ->
		    logger:debug("memory store: ~p <- ~p",[Address,Value]),
		    PID ! ok,
		    memory(array:set(max(0,Address),Value,Memory))
	    end
    after
	TimeOut ->
	    logger:notice("memory timeout",[]),
	    timeout
    end.

derive_address(PIDM,Globals,Code) ->
    logger:info("derive_address: Globals: ~p",[Globals]),
    L = if 
	    Code =:= "zero" ->
		logger:info("!!!! derive_address zero"),
		["zero","zero","zero"];
	    true ->
		lists:foldl(fun(S,Acc) -> Acc++string:split(S,"\)") 
			    end, [],string:split(Code,"\("))
	end,
    [Prefix,Label,_] = L,
    logger:debug("derive_address ~p, ~p, (~p)",[Prefix,Label,L]),
    case { lists:member(Label,rvsutils:registernames(32)), Prefix } of
	{true,_} -> 
	    Offs = case string:to_integer(Prefix) of
		       {N,[]} -> N;
		       _R -> 
			   logger:error("Prefix is no integer ~p",[_R]),
			   throw({"Prefix is no integer"})
		   end,
	    TimeOut = 100,
	    maps:get(registers,PIDM) ! {self(), load, Label },
	    logger:debug("try load register ... ~p",[Label]),
	    receive
		{ok,Val} ->
		    logger:debug(" .. loaded ~p <- ~p",[Label,Val+Offs]),
		    {absval, Val+Offs}
	    after
		TimeOut ->
		    timeout
	    end;
	{_,"zero"} -> 0;
	{_,_} -> derive_address(Globals,Label,addr,Prefix)
    end.

derive_address(Globals,Label,Key,Prefix)->
    case re:run(Label,".*[+][0-9]+") of
	nomatch -> { Lab, Offset } = { Label, 0 };
	{match,_} -> 
	    [ Lab, Off ] = string:split(Label,"+"),
	    { Offset,_ } = string:to_integer(Off)
    end,
    case lists:member(Lab,maps:keys(Globals)) of
        true ->
	    Val = make_it_integer(maps:get(Key,maps:get(Lab,Globals))),
	    logger:info("derive: ~p,~p",[Prefix,Val+Offset]),
	    {Prefix,Val+Offset};
	_ ->
	    logger:error("rvsmemory, derive_address: label ~p not found in globals ~p",
			 [Label,Globals]),
	    throw({"label not found in globals",Label})
    end.

make_it_integer(Val) ->
    if 
	is_integer(Val) ->
	    Val;
	true ->
	    case string:to_integer(Val) of
		{error,_} -> throw({"rvsmemory,make_integer: but it is not an integer",Val});
		{Res,[]} -> Res;
		_Default -> throw({"rvsmemory,make_integer: strange",_Default,Val})
	    end
    end.		    		    

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
memory_access_test() ->
    TimeOutDummy = 10,
    receive
	_ -> ok
    after
	TimeOutDummy ->
	    ok
    end,
    PIDMem = spawn(rvsmemory,memory,[init,40,0]),
    PIDMem ! {self(), load, -1 },
    TimeOut = 100,
    receive
	{ok,0} -> ?assert(true),ok;
	_ -> ?assert(false)
    after
	TimeOut ->
	    ?assert(false),
	    ok
    end,
    PIDMem ! {self(), load, 44 },
    TimeOut1 = 100,
    receive
	{ok,0} -> ?assert(true),ok;
	_ -> ?assert(false)
    after
	TimeOut1 ->
	    ?assert(false)
    end,
    ?assertException(throw,_,rvscorehw:save_memory(#{memory => PIDMem},-1,0)),
    ?assertException(throw,_,rvscorehw:save_memory(#{memory => PIDMem},401,0)),
    PIDSelf = spawn(fun() -> ok end),
    PIDMem ! {PIDSelf,store,-1,17},
    exit(PIDMem,kill).
dump_memory_test() ->
    PIDReg = spawn(rvsmemory,memory,[init,40,0]),
    PIDReg ! {self(),dump},
    receive
	{ok,_Memory} ->
	    ok
    end,
    PIDReg ! kill.
timeout_memory_test()->
    PIDReg = spawn(rvsmemory,memory,[init,40,0]),
    timer:sleep(800),
    ?assertEqual(true,is_process_alive(PIDReg)),
    timer:sleep(1500),
    ?assertEqual(false,is_process_alive(PIDReg)).
derive_address_test() ->
    0 = derive_address(#{},#{},"zero"),
    PIDReg = spawn(fun() -> ok end),
    timeout = derive_address(#{registers => PIDReg},#{},"0(a0)"),
    ?assertException(throw,_,derive_address(#{},#{},"x(a0)")),
    ?assertException(throw,_,derive_address(#{},"buffer",key,"%lo")),
    ok.
-endif.
