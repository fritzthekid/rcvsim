%% -------------------------------------------------------------------
%% @doc
%% This module provides data access utilities for the RISC-V Simulator.
%% It includes functions for binary manipulation, data conversion, and
%% memory operations.
%% -------------------------------------------------------------------

-module(rvsda).
-compile(export_all).
%%-export([new/2,numtobin/2,bintonum/2,putdata/4,put/4,getdata/3,get/3]).

%% -------------------------------------------------------------------
%% @doc
%% Creates a new binary of the specified size, filled with the default value.
%%
%% @spec new(integer(), binary()) -> binary()
%% -------------------------------------------------------------------
new(Size, Default) ->
    binary:copy(Default, Size).

%% -------------------------------------------------------------------
%% @doc
%% Extracts a byte from a value at the specified bit shift.
%%
%% @spec mask(integer(), integer()) -> integer()
%% -------------------------------------------------------------------
mask(Val, Shift) ->
    (Val bsr Shift) rem (1 bsl 8).

%% -------------------------------------------------------------------
%% @doc
%% Determines the sign of a value.
%%
%% @spec sign(integer()) -> integer()
%% -------------------------------------------------------------------
sign(V) ->
    if V >= 0 -> 1; true -> -1 end.

%% -------------------------------------------------------------------
%% @doc
%% Inverts the mask of a value at the specified bit shift.
%%
%% @spec maskinv(integer(), integer()) -> integer()
%% -------------------------------------------------------------------
maskinv(Val, Shift) ->
    255 - ((Val bsr Shift) rem (1 bsl 8)).
    
%% -------------------------------------------------------------------
%% @doc
%% Converts a number to a binary representation based on the specified type.
%%
%% @spec numtobin(integer(), atom()) -> binary()
%% -------------------------------------------------------------------
numtobin(V, Type) ->
    L = case { Type, sign(V) }  of
        { uint16, _ } ->
            [mask(V, 0), mask(V, 8)];
        { uint32, _ } ->
            [mask(V, 0), mask(V, 8), mask(V, 16), mask(V, 24)];
        { int16, 1 } ->
            [mask(V, 0), mask(V, 8)];
        { int16, -1 } ->
            [maskinv(-V, 0), maskinv(-V, 8)];
        { int32, 1 } ->
            [mask(V, 0), mask(V, 8), mask(V, 16), mask(V, 24)];
        { int32, -1 } ->
            [maskinv(-V, 0), maskinv(-V, 8), maskinv(-V, 16), maskinv(-V, 24)];
        { uint8, _ } ->
            [mask(V, 0)];
        { char, _ } ->
            [mask(V, 0)];
        _R -> throw({"Type not known:", _R})
    end,
    binary:list_to_bin(L).

%% -------------------------------------------------------------------
%% @doc
%% Places the specified value into the binary at the specified position.
%%
%% @spec putdata(binary(), integer(), integer(), atom()) -> binary()
%% -------------------------------------------------------------------
putdata(Bin, Pos, Val, Type) ->
    ValBytes = numtobin(Val, Type),
    put(Bin, Pos, ValBytes, byte_size(ValBytes)).

%% -------------------------------------------------------------------
%% @doc
%% Inserts a value into a binary at the specified position and length.
%%
%% @spec put(binary(), integer(), binary(), integer()) -> binary()
%% -------------------------------------------------------------------
put(Bin, Pos, Val, Len) ->
    Bin1Size = byte_size(Bin) - Pos - Len,
    <<Bin0:Pos/binary, _:Len/binary, Bin1:Bin1Size/binary>> = Bin,
    <<Bin0/binary, Val/binary, Bin1/binary>>.

%% -------------------------------------------------------------------
%% @doc
%% Returns the length in bytes for the specified type.
%%
%% @spec lenoftype(atom()) -> integer()
%% -------------------------------------------------------------------
lenoftype(Type) ->
    case maps:find(Type, #{char => 1, int16 => 2, uint16 => 2, int32 => 4, uint32 => 4, int => 4, uint8 => 1}) of
        error ->
            throw({"Type not known:", Type});
        {ok, Len} -> Len
    end.

%% -------------------------------------------------------------------
%% @doc
%% Converts a binary to a number based on the specified type.
%%
%% @spec bintonum(binary(), atom()) -> integer()
%% -------------------------------------------------------------------
bintonum(Bin, Type) ->
    Len = lenoftype(Type),
    Bytes = getlist(Bin, 0, Len),
    case Type of
        char -> hd(Bytes);
        uint8 -> hd(Bytes);
        uint16 -> [L, H] = Bytes, H * 256 + L;
        int16 -> [L, H] = Bytes, 
            Sign = if H band (1 bsl 7) > 0 -> -1; true -> 1 end,
            if Sign > 0 -> H * 256 + L; true -> -(1 bsl 16 - 1) + (H * 256 + L) end;
        uint32 -> [L, ML, MH, H] = Bytes, (H bsl 24) + (MH bsl 16) + (ML bsl 8) + L;
        int32 -> [L, ML, MH, H] = Bytes,
            Sign = if H band (1 bsl 7) > 0 -> -1; true -> 1 end,
            if Sign < 0 -> 
                -(1 bsl 32 - 1) + ((H bsl 24) + (MH bsl 16) + (ML bsl 8) + L);
                true -> 
                (H bsl 24) + (MH bsl 16) + (ML bsl 8) + L
            end;
        _R -> throw({"Type not known:", _R})
    end.
    
%% -------------------------------------------------------------------
%% @doc
%% Retrieves data from a binary at the specified position and type.
%%
%% @spec getdata(binary(), integer(), atom()) -> integer()
%% -------------------------------------------------------------------
getdata(Bin, Pos, Type) ->
    Len = lenoftype(Type),
    Bytes = getlist(Bin, Pos, Len),
    bintonum(binary:list_to_bin(Bytes), Type).

%% -------------------------------------------------------------------
%% @doc
%% Extracts a value from the binary at the specified position and length.
%%
%% @spec get(binary(), integer(), integer()) -> binary()
%% -------------------------------------------------------------------
get(Bin, Pos, Len) ->
    TLen = byte_size(Bin) - Pos - Len,
    <<_H:Pos/binary, Val:Len/binary, _T:TLen/binary>> = Bin,
    Val.

%% -------------------------------------------------------------------
%% @doc
%% Converts a binary segment to a list of bytes.
%%
%% @spec getlist(binary(), integer(), integer()) -> list()
%% -------------------------------------------------------------------
getlist(Bin, Pos, Len) ->
    binary:bin_to_list(get(Bin, Pos, Len)).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------------------------------
%% @doc
%% Handles memory operations for testing.
%%
%% @spec memory(binary()) -> ok
%% -------------------------------------------------------------------
memory(Memory) ->
    TimeOut = 1000,
    receive
        {PID, put, Pos, Val, Type} ->
            Mem = putdata(Memory, Pos, Val, Type),
            V = getdata(Mem, Pos, Type),
            if V =/= Val -> logger:notice("memory: diff ~p, ~p =/= ~p", [Pos, Val, V]);
               true -> ok
            end,
            PID ! ok,
            memory(Mem);
        {PID, dump} ->
            PID ! {ok, Memory},
            memory(Memory)
    after
        TimeOut ->
            throw("memory timeout")
    end.

%% -------------------------------------------------------------------
%% @doc
%% Tests load and store functions for RVSDA.
%%
%% @spec rvsda_load_store_test() -> ok
%% -------------------------------------------------------------------
rvsda_load_store_test() ->
    %% Bin0 = new(1 bsl 16, <<0>>),
    Len = 1 bsl 16,
    Bin1 = rand:bytes(Len),
    PID = spawn(rvsda, memory, [new(Len, <<0>>)]),
    lists:foreach(fun(I) ->
                      TimeOut = 100,
                      PID ! {self(), put, I, getdata(Bin1, I, int32), int32},
                      receive
                          ok -> ok;
                          _R -> ?assert(false)
                      after
                          TimeOut -> throw("Timeout waiting for memory")
                      end
                  end, lists:seq(0, Len - 1, 4)),
    PID ! {self(), dump},
    TimeOut = 100,
    Bin0 = receive
               {ok, Mem} ->
                   Mem;
               _R -> ?assert(false)
           after
               TimeOut -> throw("Timeout Dump")
           end,
    logger:notice("Bin0 Size: ~p,Bin1 Size ~p", [byte_size(Bin0), byte_size(Bin1)]),
    ?assertEqual(Bin0, Bin1),
    exit(PID, kill),
    ok.

%% -------------------------------------------------------------------
%% @doc
%% Tests number conversion functions for RVSDA.
%%
%% @spec rvsda_number_test() -> ok
%% -------------------------------------------------------------------
rvsda_number_test() ->
    Bin = new(10, <<0>>),
    lists:foreach(fun(I) ->
                      ?assertEqual(I, getdata(putdata(Bin, 4, I, int16), 4, int16))
                  end, lists:seq(-(1 bsl 15) + 1, (1 bsl 15) - 1)),
    lists:foreach(fun(I) ->
                      ?assertEqual(I, getdata(putdata(Bin, 4, I, int32), 4, int32))
                  end, lists:seq(-(1 bsl 18) + 1, (1 bsl 18) - 1)),
    ?assertEqual(65535, bintonum(numtobin(65535, uint16), uint16)),
    ?assertEqual(0, bintonum(numtobin(65535, int16), int16)),
    ?assertEqual((1 bsl 32) - 1, bintonum(numtobin((1 bsl 32) - 1, uint32), uint32)),
    ?assertEqual(0, bintonum(numtobin((1 bsl 32) - 1, int32), int32)),
    ?assertEqual(255, bintonum(numtobin(255, uint8), uint8)),
    ?assertEqual(255, bintonum(numtobin(255, char), char)),
    ?assertEqual(0, bintonum(numtobin(256, uint8), uint8)),
    ?assertException(throw, _, numtobin(100, int7)),
    ?assertException(throw, _, bintonum(<<7>>, int7)),
    ok.
-endif.
