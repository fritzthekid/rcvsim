-module(rvsutils).
-compile(export_all).

%% From: (opposite to file:consult) https://zxq9.com/archives/1021
write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = unicode:characters_to_binary(lists:map(Format, List)),
    file:write_file(Filename, Text).

code_to_object(X) ->
    Split = if is_list(X) -> 
			  re:split(X,"[\(\)]",[{return, list}]);
		     true -> [" "]
		end,
    Len = length(Split),
    %%io:format("~p~n",[{is_integer(X),is_list(X),string:to_integer(X),Len,hd(Split)}]),
    case {is_integer(X),is_list(X),string:to_integer(X),Len,hd(Split)} of
	{true,false,{error,_},_,_} ->
	    {integer,X,0};
	{false,true,{error,_},1,"zero"} ->
	    {integer,0,0};
	{false,true,{error,_},1,_} ->
	    {register,X,0};
	{false,true,{Val,[]},1,_} ->
	    {integer,Val,0};
	{false,true,{error,_},3,"%hi"} ->
	    [_|[G|_]] = Split,
	    {memory_access_via_global_hi,0,G};
	{false,true,{error,_},3,"%lo"} ->
	    [_|[G|_]] = Split,
	    {memory_access_via_global_lo,0,G};
	{false,true,{error,_},3,_} ->
	    logger:error("neither global memory(%hi,%lo) nor register access ~p",[X]),
	    {error,"neither relative memory nor register access",X};
	{false,true,{Ofs,_},3,_} ->
	    [_|[G|_]] = Split,
	    Regs = registernames(32),
	    GIsReg = lists:member(G,Regs),
	    if 
		GIsReg ->
		    {memory_access_via_register,Ofs,G};
		true ->
		    {error,"neither relative memory nor register access",X}
	    end;
	{false,true,_,5,"%lo"} ->
	    logger:debug("rvsutils:code_to_object, memory access short hand ~p",[Split]),
	    [_,G,_,Reg,_] = Split,
	    Regs = registernames(32),
	    case lists:member(Reg,Regs) of
		true ->
		    {memory_access_via_register_short_hand_lo,G,Reg};
		_ ->
		    logger:error("rvsutils:code_to_object short hand, but not register ~p",[Reg]),
		    throw({"rvsutils:code_to_object short hand, but not register",Reg})
	    end;
	_R ->
	    {error,"neither relative register nor global access",X}
    end.

factorial(Num) -> 
    if
	Num < 1   -> 0;
	Num =:= 1 -> 1;
	true      -> Num * factorial(Num-1)
    end.

get_in_list(N,L) ->
    if 
	N =:= 0 ->
	    hd(L);
	N<0 ->
	    hd(lists:sublist(L,max(1,length(L)+N+1),1));
	true ->
	    hd(lists:sublist(L,min(N,length(L)),1))
    end.

registernames(N) ->
    %% lists:foldl(fun(X,Acc) -> 
    %% 			[io_lib:format("a~2..0B", [X])] ++ Acc 
    %% 		end, [], lists:seq(0,N-1))++["s0","sp"].
    TRegs = lists:foldl(fun(I,Acc) ->
			Acc++["t"++integer_to_list(I)]
		end, [], lists:seq(0,8)),
    SRegs = lists:foldl(fun(I,Acc) ->
			Acc++["s"++integer_to_list(I)]
		end, [], lists:seq(0,15)),
    lists:foldl(fun(I,Acc) ->
			Acc++["a"++integer_to_list(I)]
		end, [], lists:seq(0,N-1))++["s0","sp","ra"]++SRegs++TRegs.

printregisters(L) ->
    printlist(lists:foldl(fun({A,B},Acc)->if B=/=0 -> Acc++[{A,B}];true->Acc end end,[],L)).
printlist(L) ->
    lists:foreach(fun(X)->io:format("~p~n",[X]) end, L).
printmemory(M,A,E,Offs) ->
    lists:foreach(fun(I)->
			  io:format("~p:~p~n",[I,maps:get(I,maps:from_list(M))]),
			  ok 
		  end,lists:seq(A,E,Offs)).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
write_terms_test() ->
    {ok, [Config]} = file:consult("data/rvs.config"),
    write_terms("test/outputs/rvs.config",[Config]),
    {ok, [MyConfig]} = file:consult("test/outputs/rvs.config"),
    ?assert(Config==MyConfig),
    ok.
code_to_object_test() ->
    ?assertEqual({integer,1,0},code_to_object(1)),
    ?assertEqual({integer,1,0},code_to_object("1")),
    ?assertEqual({memory_access_via_register,44,"sp"},code_to_object("44(sp)")),
    ?assertEqual({memory_access_via_global_hi,0,"global"},code_to_object("%hi(global)")),
    ?assertEqual({memory_access_via_global_lo,0,"global"},code_to_object("%lo(global)")),
    ?assertEqual({register,"a1",0},code_to_object("a1")),
    case code_to_object("xx(common)") of
	{error,_,_} ->
	    ?assert(true);
	{_R,_,_} ->
	    ?assert(false)
    end,
    case code_to_object("xx(co") of
	{error,_,_} ->
	    ?assert(true);
	{_,_,_} ->
	    ?assert(false)
    end,
    ok.

get_in_list_test() ->
    ?assertEqual("a",get_in_list(1,["a","b",2,5])),
    ?assertEqual(2,get_in_list(3,["a","b",2,5])),
    ?assertEqual(5,get_in_list(5,["a","b",2,5])),
    ?assertEqual(5,get_in_list(-1,["a","b",2,5])),
    ?assertEqual("a",get_in_list(0,["a","b",2,5])),
    ?assertEqual("a",get_in_list(-4,["a","b",2,5])).
registernames_test() ->
    L = registernames(32),
    ?assert(lists:member("sp",L)),
    ?assert(lists:member("a0",L)),
    ?assert(lists:member("s7",L)=:=false).
printlist_test() ->
    ?assertEqual(ok,printlist(registernames(32))).
printregisters_test() ->
    L = [{a,1},{b,0}],
    ?assertEqual(ok,printregisters(L)).
-endif.
