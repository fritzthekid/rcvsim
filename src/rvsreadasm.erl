-module(rvsreadasm).
-compile(export_all).

readasm(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [_|Text] = string:split(binary:bin_to_list(Bin),"\n",all),
    Globals = grep_globals(Text),
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
    G = lists:foldl(fun(Line,Acc) ->
			case re:run(Line,"\.global") of
			    nomatch ->
				Acc;
			    {match,[{Left,Len}]} ->
				Acc + [string:strip(lists:sublist(Line,Left+Len+1))];
			    _Default ->
				Acc + [error]
			end
		    end, [], Text),
    GS = size_of_globals(Text,G).

size_of_globals(Text,Globals) ->
    size_of_globals([],Globals,Text).
size_of_globals(Acc,[],Text) -> Acc;
size_of_globals(OAcc,[G|T],Text) ->
    NAcc = lists:foldl(fun(Line,Acc)->
			case re:run(Line,"\.size[\t, ]*"++G) of
			    nomatch ->
				Acc;
			    {match,[{Left,Len}]} ->
				Acc + [{G,string:strip(lists:sublist(Line,Left+Len+1))}];
			    _Default ->
				Acc + [error]
			end
		       end, [],Text),
    size_of_globals(NAcc,T,Text).

