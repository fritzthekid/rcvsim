-module(rvsmemory).
-compile(export_all).

memory(init,Size,Filling) ->
    memory(array:new(Size,{default,Filling})).

memory(Memory) ->
    TimeOut = 1000,
    receive
	kill ->
	    io:format("memory killed~n",[]),
	    ok;
	{ PID, dump } ->
	    PID ! {ok,Memory},
	    memory(Memory);
	{ PID, load, Address } ->
	    io:format("memory load: ~p~n",[Address]),
	    PID ! {array:get(Address,Memory)},
	    memory(Memory);
	{ PID, store, Address, Value } ->
	    io:format("memory store: ~p: ~p~n",[Address,Value]),
	    PID ! ok,
	    memory(array:set(Address,Value,Memory))
    after
	TimeOut ->
	    io:format("memory timeout~n",[]),
	    timeout
    end.

derive_address(Globals,Code) ->
    L = lists:foldl(fun(S,Acc) -> Acc++string:split(S,"\)") 
		    end, [],string:split(Code,"\(")),
    [RAdd,Label,_] = L,
    derive_address(Globals,Label,address,RAdd).

derive_address(Globals,Label,Key,RAdd)-> 
    RNum = case string:to_integer(RAdd) of
	      {error,_Reason} ->
		  logger:error("~p: ~p, (~p: ~p)",[RAdd,_Reason,?FILE,?LINE]),
		  throw("RAdd no integer");
	      {Num,_} -> 
		   if is_integer(Num) ->
			   Num;
		      true ->
			   logger:error("~p not an Integer",[RAdd]),
			   throw("RAdd no integer")
		   end
	   end,
    IsMem = lists:member(Label,maps:keys(Globals)),
    Result = if 
	IsMem ->
	    IssMem = lists:member(Key, maps:keys((maps:get(Label,Globals)))),
	    if 
		IssMem ->
		    Val = maps:get(Key,maps:get(Label,Globals)),
		    %%io:format("found: ~p,~p~n",[Val,RNum]),
		    Val+RNum;
		true ->
		    logger:error("key ~p not found in globals ~p",[Key,Globals]),
		    {error,notfound}
	    end;
	true ->
	    logger:error("label ~p not found",[Label]),
	    {error,notfound}
	     end,
    Result.

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
derive_address_test() ->
    logger:info("start derive_address_test",[]),
    Globals = maps:from_list([{"buffer",maps:from_list([{address,4000}])},
    			      {"some",maps:from_list([{type,"function"}])}]),
    logger:info("Globals ~p",[Globals]),
    Res = derive_address(Globals,"40(buffer)"),
    logger:info("Res ~p",[Res]),
    ?assertEqual(4040,Res),
    ?assertEqual({error,notfound},derive_address(Globals,"40(bufferx)")),
    ?assertEqual({error,notfound},derive_address(Globals,"40(some)")),
    ok.
store_test() ->
    PIDReg = spawn(rvsmemory,memory,[init,1000,0]),
    PIDReg ! {self(),store,17,5},
    TimeOut = 1200,
    receive
	ok ->
	    ok
    after
	TimeOut ->
	    ?assert(false)
    end,
    PIDReg ! {self(),load,17},
    receive
	{Val} ->
	    ?assertEqual(5,Val)
    after
	TimeOut ->
	    ?assert(false)
    end.
dump_memory_test() ->
    PIDReg = spawn(rvsmemory,memory,[init,40,0]),
    PIDReg ! {self(),dump},
    receive
	{ok,Memory} ->
	    ok
    end,
    PIDReg ! kill.
timeout_memory_test()->
    PIDReg = spawn(rvsmemory,memory,[init,40,0]),
    timer:sleep(600),
    ?assertEqual(true,is_process_alive(PIDReg)),
    timer:sleep(600),
    ?assertEqual(false,is_process_alive(PIDReg)).
-endif.
