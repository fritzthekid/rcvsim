-module(rvsreadasm).
-compile(export_all).

readasm(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [_|Text] = string:split(binary:bin_to_list(Bin),"\n",all),
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
