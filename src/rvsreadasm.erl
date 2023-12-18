-module(rvsreadasm).
-compile(export_all).

readasm(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [_|Text] = string:split(binary:bin_to_list(Bin),"\n",all),
    _Globals = grep_globals(Text),
    TT = lists:foldl(fun(L,Acc) -> Acc++string:strip(string:replace(L,"\t"," ")) end,[],Text),
    NT = lists:foldl(fun(L,Acc) -> Len=length(string:strip(L)), 
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
		     end, [],TT),
    %% lists:foreach(fun(Line) -> 
    %% 			  io:format("~p~n",[Line])
    %% 		  end ,NT),
    NT.

just_text(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [_|Text] = string:split(binary:bin_to_list(Bin),"\n",all),
    Text.

just_globals_and_size(Text) ->
    G=grep_globals(Text),
    SM=maps:from_list(size_of_globals(Text,G)),
    TM=maps:from_list(type_of_globals(Text,G)),
    maps:from_list(lists:foldl(fun(SG,Acc)->
				       Acc++[{SG,maps:from_list(
						   [{size,maps:get(SG,SM)},
						    {type,maps:get(SG,TM)}]
						  )
					     }
					    ] 
			       end, [], G)).

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
		{Int,_} = string:to_integer(S),
		if is_integer(Int) -> Acc++[Int];
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
				logger:info("{Left:~p,Len:~p}, match: ~s",[Left,Len,Line]),
				Acc ++ [string:strip(lists:sublist(Line,Left+Len+2,80))];
			    _Default ->
				logger:info("_Default",[Line]),
				Acc ++ [error]
			end
		    end, [], Text).
    
    %GS = size_of_globals(Text,G),
    %{G,GS}.


size_of_globals(Text,Globals) ->
    size_of_globals([],Globals,Text).
size_of_globals(Acc,[],_) -> Acc;
size_of_globals(OAcc,[G|T],Text) ->
    NAcc = lists:foldl(fun(Line,Acc)->
			case re:run(Line,"\.size[\t, ]*"++G++"[,\t ]*") of
			    nomatch ->
				Acc;
			    {match,[{Left,Len}]} ->
				logger:info("~p,~p: ~s",[Left,Len,Line]),
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
				logger:info("~p,~p: ~s",[Left,Len,Line]),
				Acc ++ [{G,string:strip(lists:sublist(Line,Left+Len+1,80))}];
			    _Default ->
				Acc ++ [error]
			end
		       end, OAcc,Text),
    type_of_globals(NAcc,T,Text).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
globals_test() ->
    G = just_globals_and_size(just_text("data/func-with-globals.s")),
    ?assertEqual("4000",maps:get(size,maps:get("buffer",G))),
    ok.
list_text_test() ->
    Text = just_text("data/func-with-globals.s"),
    ?assertEqual(19,length(list_text(Text,3,22))),
    print_text(Text).
-endif.
