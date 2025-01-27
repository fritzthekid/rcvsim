%% -------------------------------------------------------------------
%% @doc
%% This module manages the memory operations for the RISC-V Simulator.
%% It handles memory initialization, loading, storing, and address
%% derivation.
%% -------------------------------------------------------------------

-module(rvsmemory).
-compile(export_all).

%% Initialize memory with a given size, filled with zeros
memory(init, Size) ->
    memory(binary:copy(<<0>>, Size)).

%% Initialize memory with a given size and filling (not used)
memory(init, Size, _Filling) ->
    memory(init, Size).

%% Main memory process to handle memory operations
memory(Memory) ->
    TimeOut = 2000,
    receive
        kill ->
            io:format("memory killed~n", []),
            ok;
        {PID, dump} ->
            PID ! {ok, Memory},
            memory(Memory);
        {PID, load, Address, Len} ->
            logger:debug("memory load address ~p", [Address]),
            if 
                Address < 0 ->
                    logger:warning("load memory at address ~p", [Address]),
                    PID ! {ok, 0};
                true ->
                    Size = byte_size(Memory),
                    if Address + Len < Size ->
                        Value = rvsda:get(Memory, Address, Len),
                        logger:debug("memory load: address ~p: (get value ~p)", [Address, Value]),
                        PID ! {ok, Value};
                       true -> 
                        logger:error("memory load: address ~p > ~p", [Address, Size]),
                        PID ! {ok, 0}
                    end
            end,
            memory(Memory);
        {PID, store, Address, Value} ->
            Len = byte_size(Value),
            {Lower, Higher} = {(Address < 0), (Address >= (byte_size(Memory) + Len))},
            logger:info("rvsmemory: memory access is (< 0): ~p or (>=Size): ~p", [Lower, Higher]),
            case {Lower, Higher} of
                {true, _} ->
                    logger:error("memory access out of range < 0: ~p", [Address]),
                    throw({"memory access out of range", Address});
                {_, true} ->
                    logger:error("memory access out of range >= Size: ~p", [Address]),
                    throw({"memory access out of range", Address});
                {_, _} ->
                    logger:debug("memory store: ~p <- ~p", [Address, Value]),
                    PID ! ok,
                    Bin1Size = byte_size(Memory) - Address - Len,
                    <<Bin0:Address/binary, _:Len/binary, Bin1:Bin1Size/binary>> = Memory,
                    memory(<<Bin0/binary, Value/binary, Bin1/binary>>)
            end
    after
        TimeOut ->
            logger:notice("memory timeout", []),
            timeout
    end.

%% Derive an address from a given code
derive_address(PIDM, Globals, Code) ->
    L = if 
            Code =:= "zero" ->
                logger:info("!!!! derive_address zero"),
                ["zero", "zero", "zero"];
            true ->
                lists:foldl(fun(S, Acc) -> Acc ++ string:split(S, "\)") 
                            end, [], string:split(Code, "\("))
        end,
    [Prefix, Label, _] = L,
    logger:debug("derive_address ~p, ~p, (~p)", [Prefix, Label, L]),
    case {lists:member(Label, rvsutils:registernames(32)), Prefix} of
        {true, _} -> 
            Offs = case string:to_integer(Prefix) of
                       {N, []} -> N;
                       _R -> 
                           logger:error("Prefix is no integer ~p", [_R]),
                           throw({"Prefix is no integer"})
                   end,
            TimeOut = 100,
            maps:get(registers, PIDM) ! {self(), load, Label},
            logger:debug("try load register ... ~p", [Label]),
            receive
                {ok, Val} ->
                    logger:debug(".. loaded ~p <- ~p", [Label, Val + Offs]),
                    {absval, Val + Offs}
            after
                TimeOut ->
                    timeout
            end;
	{_, "zero"} -> 0;
        {_, _} -> derive_address(Globals, Label, addr, Prefix)
    end.

%% Derive an address from globals, label, key, and prefix
derive_address(Globals, Label, Key, Prefix) ->
    {Lab, Offset} = case re:run(Label, ".*[+][0-9]+") of
                        nomatch -> {Label, 0};
                        {match, _} -> 
                            [La, Off] = string:split(Label, "+"),
                            {Offs, _} = string:to_integer(Off),
                            {La, Offs}
                    end,
    case lists:member(Lab, maps:keys(Globals)) of
        true ->
            Val = make_it_integer(maps:get(Key, maps:get(Lab, Globals))),
            {Prefix, Val + Offset};
        _ ->
            throw({"label not found in globals", Label})
    end.

%% Convert a value to integer if it is not already an integer
make_it_integer(Val) ->
    if 
        is_integer(Val) ->
            Val;
        true ->
            case string:to_integer(Val) of
                {error, _} -> throw({"rvsmemory, make_it_integer: not an integer", Val});
                {Res, []} -> Res;
                _Default -> throw({"rvsmemory, make_it_integer: strange", _Default, Val})
            end
    end.		    		    

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").

%% Test for memory access
memory_access_test() ->
    TimeOutDummy = 10,
    receive
        _ -> ok
    after
        TimeOutDummy -> ok
    end,
    PIDMem = spawn(rvsmemory, memory, [init, 40]),
    PIDMem ! {self(), load, -1, 1},
    TimeOut = 100,
    receive
        {ok, 0} -> ?assert(true), ok;
        _ -> ?assert(false)
    after
        TimeOut -> ?assert(false), ok
    end,
    PIDMem ! {self(), load, 44, 4},
    TimeOut1 = 100,
    receive
        {ok, 0} -> ?assert(true), ok;
        _ -> ?assert(false)
    after
        TimeOut1 -> ?assert(false)
    end,
    ?assertException(throw, _, rvscorehw:save_memory(#{memory => PIDMem}, -1, 0)),
    ?assertException(throw, _, rvscorehw:save_memory(#{memory => PIDMem}, 401, 0)),
    PIDSelf = spawn(fun() -> ok end),
    PIDMem ! {PIDSelf, store, -1, <<17>>},
    exit(PIDMem, kill).

%% Test for dumping memory
dump_memory_test() ->
    PIDMem = spawn(rvsmemory, memory, [init, 40]),
    PIDMem ! {self(), dump},
    receive
        {ok, _Memory} -> ok
    end,
    PIDMem ! kill.

%% Test for memory timeout
timeout_memory_test() ->
    PIDReg = spawn(rvsmemory, memory, [init, 40]),
    timer:sleep(800),
    ?assertEqual(true, is_process_alive(PIDReg)),
    timer:sleep(1500),
    ?assertEqual(false, is_process_alive(PIDReg)).

%% Test for deriving address
derive_address_test() ->
    0 = derive_address(#{}, #{}, "zero"),
    PIDReg = spawn(fun() -> ok end),
    timeout = derive_address(#{registers => PIDReg}, #{}, "0(a0)"),
    ?assertException(throw, _, derive_address(#{}, #{}, "x(a0)")),
    ?assertException(throw, _, derive_address(#{},"buffer", key, "%lo")),
    ok.

-endif.
