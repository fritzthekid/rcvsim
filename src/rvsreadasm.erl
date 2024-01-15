-module(rvsreadasm).
-compile(export_all).

readasm(Filename) ->
    Text = remove_comment(read_text_file_as_list(Filename)),
    rvsutils:write_terms("bck/rawprog_as_list.s",Text),
    Globals = globals_maps(Text,"data/global-address-list.config"),
    {_,Labs,Code} = split_labels_code(Text),
    {Code,{Globals,maps:from_list(Labs)}}.

read_text_file_as_list(Filename) ->
    [_|Text] = case file:read_file(Filename) of
		   {ok, Bin} -> 
		       string:split(binary:bin_to_list(Bin),"\n",all);
		   {error,enoent} ->
		       logger:error("File not found, ~p",[Filename]),
		       [error,error];
		   {error,Reason} ->
		       logger:error("Error: ~p, ~p",[Reason,Filename]),
		       [error,error]
	       end,
    Text.

split_labels_code(Text) ->
    lists:foldl(fun split_line_labels_code/2,{0,[],[]},Text).
split_line_labels_code(L,{I,Labs,Code}) ->
    IsLabel = re:run(L,"^[\.a-zA-Z][a-zA-Z0-9_]+:"),
    IsCode = re:run(L,"^\t[a-z].*"),
    IsDirective = re:run(L,"^\t[\.].*"),
    case {IsLabel,IsCode,IsDirective} of
	{nomatch,nomatch,{match,_}} ->
	    {I,Labs,Code};
	{nomatch,{match,_},nomatch} ->
	    {I+1,Labs,Code++[{I,do_line(L)}]};
	{{match,_},nomatch,nomatch} ->
	    {I,Labs++[{hd(string:split(L,":")),I}],Code};
	_Default ->
	    logger:info("line no directive or code or comment: ~p",[L]),
	    {I,Labs,Code}
    end.

remove_comment(Text) ->
    [H|T] = Text,
    lists:foldl(fun(L,Acc) ->
			case re:run(L,"^[ ]*#.*") of
			    {match,_} ->
				Acc;
			    nomatch ->
				Acc ++ [L]
			end
		end, [H], T).

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
    case hd(L) of
	9 ->
	    [_|T] = L;
	_ ->
	    T=L
    end,
    case re:run(T,"\t") of
	nomatch ->
	    do_clean_line(string:strip(T));
	_ ->
	    do_clean_line(string:strip(string:replace(T,"\t"," ",all)))
    end.
do_clean_line(L) ->
    logger:debug("clean line: ~p",[L]),
    LL=string:split(L,",",all),
    LLL=lists:foldl(fun(S,Acc)->Acc++string:split(S," ") end,[],LL),
    LLLL=lists:foldl(fun(S,Acc)->Acc++[string:strip(S)] end,[],LLL),
    F = fun(S,Acc) ->
		case string:to_integer(S) of
		    {error,_} -> Acc++[S];
		    {Int,[]} -> Acc++[Int];
		    {_,_} -> Acc++[S]
		end
	end,
    lists:foldl(F,[],LLLL).

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
    rvsutils:write_terms("test/outputs/no-globals.config",[[]]),
    G = globals_maps(read_text_file_as_list("_build/obj/func-with-globals.s"),"test/outputs/no-globals.config"),
    ?assertEqual("4000",maps:get(size,maps:get("buffer",G))),
    ok.
list_text_test() ->
    Text = read_text_file_as_list("_build/obj/func-with-globals.s"),
    ?assertEqual(19,length(list_text(Text,3,22))),
    print_text(Text).
read_text_file_test() ->
    [error] = read_text_file_as_list("test/x/somefile").
globals_maps_test() ->
    GM = globals_maps(read_text_file_as_list("_build/obj/func-with-globals.s"),
		     "test/data/no-globals.config"),
    GX = globals_maps(read_text_file_as_list("_build/obj/func-with-globals.s"),
		      "test/x/somefile"),
    ?assert(GM==GX).
do_line_test()->
    ?assertEqual(["nop"],do_line("\tnop")),
    ?assertEqual(["nop"],do_line("nop")),
    ?assertEqual(["nop","nop"],do_line("\tnop   nop")),
    ?assertEqual(["nop","nop"],do_line("\tnop\t   nop")),
    ok.
-endif.
