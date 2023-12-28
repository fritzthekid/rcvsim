-module(rvsreadasm).
-compile(export_all).

readasm(Filename) ->
    Text = read_text_file_as_list(Filename),
    rvsutils:write_terms("bck/rawprog_as_list.s",Text),
    Globals = globals_maps(Text,"data/global-address-list.config"),
    TT = lists:foldl(fun(L,Acc) -> Acc++string:strip(string:replace(L,"\t"," ")) end,[],Text),
    { lists:foldl(fun(L,Acc) -> Len=length(string:strip(L)), 
				   if Len > 0 -> 
					   Is = (hd(string:strip(L)) =/= 46),
					   if
					       Is ->
						   Acc++[do_line(L)];
					       true ->
						   Acc
					   end;
				      true -> Acc
				   end
		     end, [],TT), Globals }.

read_text_file_as_list(Filename) ->
    [_|Text] = case file:read_file(Filename) of
		   {ok, Bin} -> 
		       string:split(binary:bin_to_list(Bin),"\n",all);
		   {error,enoent} ->
		       logger:error("File not found, ~p",[Filename]),
		       [error];
		   {error,Reason} ->
		       logger:error("Error: ~p, ~p",[Reason,Filename]),
		       [error]
	       end,
    Text.

globals_maps(Text,GlobalsFilename) ->
    G=grep_globals(Text),
    SM=maps:from_list(size_of_globals(Text,G)),
    TM=maps:from_list(type_of_globals(Text,G)),
    GM=maps:from_list(lists:foldl(fun(SG,Acc)->
				       Acc++[{SG,maps:from_list(
						   [{size,maps:get(SG,SM)},
						    {type,maps:get(SG,TM)}]
						  )
					     }
					    ] 
				  end, [], G)),
    case file:consult(GlobalsFilename) of
	{ok,[L]} ->
	    append_key_values(GM,L);
	{error,Reason} ->
	    logger:error("reading file (~s): ~p",[GlobalsFilename,Reason]),
	    GM
end.

append_key_values(M,[])-> M;
append_key_values(M,[{G,K,V}|T]) ->
    append_key_values(append_key_values(M,G,K,V),T).

append_key_values(M1,G,K,V) ->
    case maps:find(G,M1) of
	{ok,_} ->
	    maps:update(G,maps:merge(maps:get(G,M1),#{K=>V}),M1);
	error ->
	    maps:put(G,#{K => V},M1)
    end.
	
print_text(Text) ->
    lists:foreach(fun({I,L}) -> io:format("~p: ~s~n",[I,L]) end, lists:zip(lists:seq(1,length(Text)),Text)).

list_text(Text,A,E) ->
    lists:foldl(fun({I,L},Acc) -> IsIn = ((I>=A) and (I<E)), 
				  if 
				      IsIn -> 
					  Acc++[L]; 
				      true -> 
					  Acc 
				  end
		end, [],lists:zip(lists:seq(1,length(Text)),Text)).

do_line(L) ->
    LL=string:split(string:replace(L,"\t"," "),",",all),
    LLL=lists:foldl(fun(S,Acc)->Acc++string:split(S," ") end,[],LL),
    LLLL=lists:foldl(fun(S,Acc)->Acc++[string:strip(S)] end,[],LLL),
    F = fun(S,Acc) ->
		{Int,T} = string:to_integer(S),
		IsInt = (Int=/=error),
		if IsInt ->
			IsTInt = (length(T)=:=0),
			if IsTInt -> Acc++[Int];
			   true -> Acc++[S]
			end;
		   true -> Acc++[S]
		end
	end,
    list_to_tuple(lists:foldl(F,[],LLLL)).

grep_globals(Text) ->
    lists:foldl(fun(Line,Acc) ->
			case re:run(Line,"\.globl") of
			    nomatch ->
				Acc;
			    {match,[{Left,Len}]} ->
				Acc ++ [string:strip(lists:sublist(Line,Left+Len+2,80))];
			    _Default ->
				Acc ++ [error]
			end
		    end, [], Text).

size_of_globals(Text,Globals) ->
    size_of_globals([],Globals,Text).
size_of_globals(Acc,[],_) -> Acc;
size_of_globals(OAcc,[G|T],Text) ->
    NAcc = lists:foldl(fun(Line,Acc)->
			case re:run(Line,"\.size[\t, ]*"++G++"[,\t ]*") of
			    nomatch ->
				Acc;
			    {match,[{Left,Len}]} ->
				Acc ++ [{G,string:strip(lists:sublist(Line,Left+Len+1,80))}];
			    _Default ->
				Acc ++ [error]
			end
		       end, OAcc,Text),
    size_of_globals(NAcc,T,Text).

type_of_globals(Text,Globals) ->
    type_of_globals([],Globals,Text).
type_of_globals(Acc,[],_) -> Acc;
type_of_globals(OAcc,[G|T],Text) ->
    NAcc = lists:foldl(fun(Line,Acc)->
			case re:run(Line,"\.type[\t, ]*"++G++"[,\t ]*") of
			    nomatch ->
				Acc;
			    {match,[{Left,Len}]} ->
				Acc ++ [{G,string:strip(lists:sublist(Line,Left+Len+1,80))}];
			    _Default ->
				Acc ++ [error]
			end
		       end, OAcc,Text),
    type_of_globals(NAcc,T,Text).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
globals_test() ->
    G = globals_maps(read_text_file_as_list("data/func-with-globals.s"),"test/data/no-globals.config"),
    ?assertEqual("4000",maps:get(size,maps:get("buffer",G))),
    ok.
list_text_test() ->
    Text = read_text_file_as_list("data/func-with-globals.s"),
    ?assertEqual(19,length(list_text(Text,3,22))),
    print_text(Text).
globals_maps_test() ->
    GM = globals_maps(read_text_file_as_list("data/func-with-globals.s"),
		     "test/data/no-globals.config"),
    GX = globals_maps(read_text_file_as_list("data/func-with-globals.s"),
		      "test/x/somefile"),
    ?assert(GM==GX).
-endif.
