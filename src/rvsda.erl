-module(rvsda).
-compile(export_all).

new(Size,Default) ->
    binary:copy(Default,Size).

mask(Val,Shift) ->
    (Val bsr Shift) rem (1 bsl 8).

sign(V) ->
    if V >= 0 -> 1; true -> -1 end.

maskinv(Val,Shift) ->
    255-((Val bsr Shift) rem (1 bsl 8)).
    
numtobin(V,Type) ->
    L = case { Type, sign(V) }  of
	{ uint16, _ } ->
	    [mask(V,0),mask(V,8)];
	{ uint32, _ } ->
	    [mask(V,0),mask(V,8),mask(V,16),mask(V,24)];
	{ int16, 1 } ->
	    [mask(V,0),mask(V,8)];
	{ int16, -1 } ->
	    [maskinv(-V,0),maskinv(-V,8)];
	{ int32, 1 } ->
	    [mask(V,0),mask(V,8),mask(V,16),mask(V,24)];
	{ int32, -1 } ->
	    [maskinv(-V,0),maskinv(-V,8),maskinv(-V,16),maskinv(-V,24)];
	{ char, _ } ->
	    [mask(V,0)];
	_R -> throw({"Type not known:",_R})
    end,
    binary:list_to_bin(L).

putdata(Bin,Pos,Val,Type) ->
    ValBytes = numtobin(Val,Type),
    put(Bin,Pos,ValBytes,byte_size(ValBytes)).

put(Bin,Pos,Val,Len) ->
    Bin1Size=byte_size(Bin)-Pos-Len,
    <<Bin0:Pos/binary,_:Len/binary,Bin1:Bin1Size/binary>> = Bin,
    <<Bin0/binary,Val/binary,Bin1/binary>>.

lenoftype(Type) ->
    maps:get(Type,#{char=>1,int16=>2,uint16=>2,int32=>4,uint32=>4,int=>4,uint8=>1}).

bintonum(Bin,Type) ->
    Len = lenoftype(Type),
    Bytes = getlist(Bin,0,Len),
    case Type of
	char -> hd(Bytes);
	uint8 -> hd(Bytes);
	uint16 -> [L,H] = Bytes, H*256+L;
	int16 -> [L,H] = Bytes, 
		 Sign = if H band (1 bsl 7) > 0 -> -1; true -> 1 end,
		 if Sign > 0 -> H*256+L; true -> -(1 bsl 16 -1)+(H*256+L) end;
	uint32 -> [L,ML,MH,H] = Bytes, (H bsl 24)+(MH bsl 16)+(ML bsl 8)+L;
	int ->   bintonum(Bin,int32);
	int32 -> [L,ML,MH,H] = Bytes,
		 Sign=if H band (1 bsl 7) > 0 -> -1; true -> 1 end,
		 %%logger:info("getdata ~p ~p ~p ~p: sign ~p",[L,ML,MH,H,Sign]),
		 if Sign < 0 -> 
			 -(1 bsl 32 - 1)+((H bsl 24)+(MH bsl 16)+(ML bsl 8)+L);
		    true -> 
			 (H bsl 24)+(MH bsl 16)+(ML bsl 8)+L
		 end;
	_R -> throw({"Type not known:",_R})
    end.
	
getdata(Bin,Pos,Type) ->
    Len = lenoftype(Type),
    Bytes = getlist(Bin,Pos,Len),
    bintonum(binary:list_to_bin(Bytes),Type).

get(Bin,Pos,Len) ->
    TLen = byte_size(Bin)-Pos-Len,
    <<_H:Pos/binary,Val:Len/binary,_T:TLen/binary>> = Bin,
    Val.

getlist(Bin,Pos,Len) ->
    binary:bin_to_list(get(Bin,Pos,Len)).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
memory(Memory) ->
    TimeOut=1000,
    receive
	{PID,put,Pos,Val,Type} ->
	    Mem = putdata(Memory,Pos,Val,Type),
	    V = getdata(Mem,Pos,Type),
	    if V =/= Val -> logger:notice("memory: diff ~p, ~p =/= ~p",[Pos,Val,V]);
	       true -> ok
	    end,
	    PID ! ok,
	    memory(Mem);
	{PID,get,Pos,Type} ->
	    Val=getdata(Memory,Pos,Type),
	    PID ! {ok,Val},
	    memory(Memory);
	{PID,dump} ->
	    PID ! {ok,Memory},
	    memory(Memory)
    after
	TimeOut ->
	    throw("memory timeout")
    end.

rvsda_load_store_test()->
    %% Bin0 = new(1 bsl 16,<<0>>),
    Len = 1 bsl 16,
    Bin1 = rand:bytes(Len),
    PID = spawn(rvsda,memory,[new(Len,<<0>>)]),
    lists:foreach(fun(I)->
			  TimeOut=100,
			  PID ! {self(),put,I,getdata(Bin1,I,int32),int32},
			  receive
			      ok -> ok;
			      _R -> ?assert(false)
			  after
			      TimeOut -> ?assert(false)
			  end
		  end,lists:seq(0,Len-1,4)),
    PID ! {self(),dump},
    TimeOut = 100,
    Bin0 = receive
	       {ok,Mem} ->
		   Mem;
	       _R -> ?assert(false)
	   after
	       TimeOut -> ?assert(false)
	   end,
    logger:notice("Bin0 Size: ~p,Bin1 Size ~p",[byte_size(Bin0),byte_size(Bin1)]),
    ?assertEqual(Bin0,Bin1),
    exit(PID,kill),
    ok.
rvsda_number_test() ->
    Bin = new(10,<<0>>),
    lists:foreach(fun(I)->
			  ?assertEqual(I,getdata(putdata(Bin,4,I,int16),4,int16))
		  end, lists:seq(-(1 bsl 15)+1,(1 bsl 15)-1)),
    lists:foreach(fun(I)->
			  ?assertEqual(I,getdata(putdata(Bin,4,I,int32),4,int32))
		  end, lists:seq(-(1 bsl 18)+1,(1 bsl 18)-1)).
-endif.
