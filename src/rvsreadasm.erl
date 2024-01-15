-module(rvsreadasm).
-compile(export_all).

readasm(Filename) ->
    Text = remove_comment(read_text_file_as_list(Filename)),
    rvsutils:write_terms("bck/rawprog_as_list.s",Text),
    Globals = globals_maps(Text,"data/global-address-list.config"),
    %%{_,NText} = lists:foldl(fun(L,{I,Acc})->{I+1,Acc+[{I,L}]} end,{0,[]},Text),
    %% Labels = maps:from_list(grep_labels(Text)),
    %% T = lists:foldl(fun(L,Acc) -> if length(L) > 0 ->
    %% 					  case hd(L) of
    %% 					      9 -> Acc ++ [L];
    %% 					      _ -> Acc
    %% 					  end;
    %% 				     true -> Acc
    %% 				  end
    %% 		    end,[],Text),
    %% TT = lists:foldl(fun(L,Acc) -> Acc++string:strip(string:replace(L,"\t"," ")) end,[],T),
    %% { lists:foldl(fun(L,Acc) -> Len=length(string:strip(L)),
    %% 				   if Len > 0 -> 
    %% 					   Is = (hd(string:strip(L)) =/= 46),
    %% 					   if
    %% 					       Is ->
    %% 						   Acc++[do_line(L)];
    %% 					       true ->
    %% 						   Acc
    %% 					   end;
    %% 				      true -> Acc
    %% 				   end
    %% 		q     end, [],TT), { Globals, Labels } }.
    {_,Labs,Code} = split_labels_code(Text),
    {Code,{Globals,maps:from_list(Labs)}}.

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

split_labels_code(Text) ->
    lists:foldl(fun split_line_labels_code/2,{0,[],[]},Text).
split_line_labels_code(L,{I,Labs,Code}) ->
    logger:debug("slc: ~p,~p,~p,~p",[L,I,Labs,Code]),
    IsLabel = re:run(L,"^[\.a-zA-Z][a-zA-Z0-9_]+:"),
    IsCode = re:run(L,"^\t[a-z].*"),
    IsDirective = re:run(L,"^\t[\.].*"),
    logger:info("~p, ~p,~p,~p",[L,IsLabel,IsCode,IsDirective]),
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

grep_labels(Text)->
    F = fun(L,{I,Acc}) ->
		case re:run(L,"^\t[a-z]+.*") of
		    {match,_} -> II = I+1; 
		    nomatch -> II = I
		end,
		%%case re:run(".Lssaf:","^[\.a-zA-Z][a-zA-Z0-9_]+:") of
		case re:run(L,"^[\.a-zA-Z][a-zA-Z0-9_]+:") of
		    {match,_} ->
			[Lab|_] = string:split(L,":"),
			{ II, Acc ++ [{Lab,I-1}] }; %% !!!!!!!!!!! Labels sind nicht korrekt!! %%

		    nomatch -> { II, Acc }
		end
	end,
    {_,Labels} = lists:foldl(F,{0,[]},Text),
    Labels.

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
globals_test() ->
    rvsutils:write_terms("test/outputs/no-globals.config",[[]]),
    G = globals_maps(read_text_file_as_list("_build/obj/func-with-globals.s"),"test/outputs/no-globals.config"),
    ?assertEqual("4000",maps:get(size,maps:get("buffer",G))),
    ok.
labels_test() ->
    Text = ["bla","fri_tz","sdfgs_f3dg:","\tload 100", "\tsw a5,100",
	    ".L7:","\tfas2daf",".as2fd","3asdfasfd","\tasdf",".L17:"],
    Labels = maps:from_list(grep_labels(Text)),
    ?assertEqual(11,length(maps:keys(Labels))),
    ?assertEqual(4,maps:get(".L17",Labels)),
    ?assertEqual(error,maps:find(".L8",Labels)),
    ?assertException(error,_,maps:get("asfd",Labels)).
list_text_test() ->
    Text = read_text_file_as_list("_build/obj/func-with-globals.s"),
    ?assertEqual(19,length(list_text(Text,3,22))),
    print_text(Text).
globals_maps_test() ->
    GM = globals_maps(read_text_file_as_list("_build/obj/func-with-globals.s"),
		     "test/data/no-globals.config"),
    GX = globals_maps(read_text_file_as_list("_build/obj/func-with-globals.s"),
		      "test/x/somefile"),
    ?assert(GM==GX).
-endif.
