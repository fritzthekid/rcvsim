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
	    if Address < 0 ->
		    logger:warning("memory store: at negativ position!!: ~p",[Address]);
	       true ->
		    logger:debug("memory store: ~p <- ~p",[Address,Value])
	    end,
	    PID ! ok,
	    memory(array:set(max(0,Address),Value,Memory))
    after
	TimeOut ->
	    logger:notice("memory timeout",[]),
	    timeout
    end.

derive_address(PIDM,Globals,Code) ->
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
	{true,_} -> maps:get(registers,PIDM) ! {self(), load, Label },
		logger:debug("try load register ... ~p",[Label]),
		Offs = case string:to_integer(Prefix) of
			   {N,[]} -> N;
			   _R -> 
			       logger:error("Prefix is no integer ~p",[_R]),
			       throw({"Prefix is no integer"})
		       end,
		TimeOut = 100,
		receive
		    {ok,Val} ->
			logger:debug(" .. loaded ~p <- ~p",[Label,Val+Offs]),
			{absval, Val+Offs}
		after
		    TimeOut ->
			error
		end;
	{_,"zero"} -> 0;
	{_,_} -> derive_address(Globals,Label,address,Prefix)
    end.

derive_address(Globals,Label,Key,Prefix)-> 
    case lists:member(Label,maps:keys(Globals)) of
        true ->
	    Val = maps:get(Key,maps:get(Label,Globals)),
	    logger:info("derive: ~p,~p",[Prefix,Val]),
	    {Prefix,Val};
	_ ->
	    logger:error("label ~p not found in globals ~p",[Label,Globals]),
	    {error,notfound}
    end.

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
%% get_globals() ->
%%     Globals = maps:from_list([{"buffer",maps:from_list([{address,500}])},
%%     			      {"some",maps:from_list([{type,"function"}])}]),
%%     Globals.
%% derive_address_test() ->
%%     Globals = get_globals(),
%% %maps:from_list([{"buffer",maps:from_list([{address,4000}])},
%% %    			      {"some",maps:from_list([{type,"function"}])}]),
%%     Res = derive_address(maps:new(),Globals,"%hi(buffer)"),
%%     ?assertEqual(0,Res),
%%     Res = derive_address(maps:new(),Globals,"%lo(buffer)"),
%%     ?assertEqual(0,500),
%%     ?assertEqual({error,notfound},derive_address(maps:new(),Globals,"40(bufferx)")),
%%     ?assertEqual({error,notfound},derive_address(maps:new(),Globals,"40(some)")),
%%     ok.
%% store_load_memory_test() ->
%%     Globals = get_globals(),
%%     PIDReg = spawn(rvsmemory,memory,[init,1000,0]),
%%     PIDReg ! {self(),store,derive_address(maps:new(),Globals,"40(buffer)"),5},
%%     TimeOut = 1200,
%%     receive
%% 	ok ->
%% 	    ok
%%     after
%% 	TimeOut ->
%% 	    ?assert(false)
%%     end,
%%     logger:info("store success",[]),
%%     PIDReg ! {self(),load,derive_address(maps:new(),Globals,"40(buffer)")},
%%     receive
%% 	{ok,Val} ->
%% 	    ?assertEqual(5,Val),
%% 	    logger:info("load succes ~p",[Val])
%%     after
%% 	TimeOut ->
%% 	    logger:info("load timeout",[]),
%% 	    ?assert(false)
%%     end.
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
    timer:sleep(600),
    ?assertEqual(true,is_process_alive(PIDReg)),
    timer:sleep(600),
    ?assertEqual(false,is_process_alive(PIDReg)).
-endif.
