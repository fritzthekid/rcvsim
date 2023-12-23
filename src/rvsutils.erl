-module(rvsutils).
-compile(export_all).

%% From: (opposite to file:consult) https://zxq9.com/archives/1021
write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = unicode:characters_to_binary(lists:map(Format, List)),
    file:write_file(Filename, Text).

string_to_integer(X) ->
    {E1,E2} = string:to_integer(X),
    if 
	E1 =:= error -> 
	    {false,X};
	length(E2) =:= 0 ->
	    {true,E1};
	true ->
	    {false,X}
    end.

string_to_integer_1(X) ->
    case {is_integer(X),is_list(X),string:to_integer(X)} of
	{true,_,{error, _}} ->
	    {number,X};
	{false,true,{error, _}} ->
	    {register,X};
	{_,true,{Val,[]}} ->
	    {integer,Val};
	{_,true,{Ofs,Ref}} ->
	    IsRel = (hd(Ref) =:= 40) and (lists:last(Ref) =:= 41),
	    if 
		IsRel ->
		    G = lists:reverse(tl(lists:reverse(tl(Ref)))),
		    IsReg = lists:member(G,["s0","sp"]),
		    if IsReg ->
			    {memory_acess_via_registr,Ofs,G};
		       true ->
			    {memory_access_via_global,Ofs,G}
		    end;
		true ->
		    logger:error("neither relative memory but neither register nor global access ~p",[X]),
		    throw({"neither register nor memory access",X})
	    end;
	_R ->
	    logger:error("neither relative memory nor register access ~p: ~p",[X,_R]),
	    throw({"neither register nor memory access",X,_R})
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

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
write_terms_test() ->
    {ok, [Config]} = file:consult("data/config.config"),
    write_terms("test/outputs/config.config",[Config]),
    {ok, [MyConfig]} = file:consult("test/outputs/config.config"),
    ?assert(Config==MyConfig),
    ok.
string_to_integer_test() ->
    ?assertEqual({true,1},string_to_integer("1")),
    ?assertEqual({false,"1(global)"},string_to_integer("1(global)")),
    ?assertEqual({false,"a1"},string_to_integer("a1")).
string_to_integer_1_test() ->
    ?assertEqual({number,1},string_to_integer_1(1)),
    ?assertEqual({integer,1},string_to_integer_1("1")),
    ?assertEqual({memory_acess_via_registr,44,"sp"},string_to_integer_1("44(sp)")),
    ?assertEqual({memory_access_via_global,1,"global"},string_to_integer_1("1(global)")),
    ?assertEqual({register,"a1"},string_to_integer_1("a1")).
get_in_list_test() ->
    ?assertEqual("a",get_in_list(1,["a","b",2,5])),
    ?assertEqual(2,get_in_list(3,["a","b",2,5])),
    ?assertEqual(5,get_in_list(5,["a","b",2,5])),
    ?assertEqual(5,get_in_list(-1,["a","b",2,5])),
    ?assertEqual("a",get_in_list(0,["a","b",2,5])),
    ?assertEqual("a",get_in_list(-4,["a","b",2,5])).
-endif.
