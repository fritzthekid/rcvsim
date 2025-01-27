%% -------------------------------------------------------------------
%% @doc
%% This module provides the main entry point and core functionalities
%% for the RISC-V Simulator, including running programs, handling I/O,
%% and managing memory and registers.
%% -------------------------------------------------------------------

-module(rvsmain).
-compile([export_all]).

%% -------------------------------------------------------------------
%% @doc
%% Terminates the given list of processes.
%%
%% @spec kill(list(pid())) -> ok
%% -------------------------------------------------------------------
kill(PIDL) ->
    lists:foreach(fun(X) -> exit(X, kill) end, PIDL).

%% -------------------------------------------------------------------
%% @doc
%% Runs the default program.
%%
%% @spec run() -> {map(), pid()}
%% -------------------------------------------------------------------
run() ->
    run("_build/obj/simple-func.s", []).

%% -------------------------------------------------------------------
%% @doc
%% Runs the specified program.
%%
%% @spec run(string()) -> {map(), pid()}
%% -------------------------------------------------------------------
run(Filename) ->
    run(Filename, []).

%% -------------------------------------------------------------------
%% @doc
%% Runs the specified program with the given configuration.
%%
%% @spec run(string(), list(tuple())) -> {map(), pid()}
%% -------------------------------------------------------------------
run(Filename, ConfigList) ->
    ROOT = "./",
    {ok, [RawConfig]} = file:consult("data/rvs.config"),
    Config = lists:foldl(fun({X, Y}, Map) -> maps:put(X, Y, Map) 
                 end, RawConfig, ConfigList ++ [{programname, Filename}]),
    {PP, Defines, Strings} = rvsreadasm:readasm(maps:get(programname, Config)),
    rvsutils:write_terms("_build/tmp/program.s", [PP]),
    rvsutils:write_terms("_build/tmp/globals_labels.config", [Defines]),
    {ok, [Data]} = file:consult(ROOT ++ "/data/" ++ maps:get(dataname, Config)),
    PIDRegs = spawn(rvscorehw, registers, [init, maps:get(registers, Config), 0]),
    PIDMem = spawn(rvsmemory, memory, [init, maps:get(memory, Config), 100]),
    PIDio = spawn(rvsio, serialio_stdout, []),
    PIDwd = spawn(rvsio, watchdog, [[PIDMem, PIDio], maps:get(timeout_main, Config) / 1000 + 2]),

    PIDM = maps:from_list([{registers, PIDRegs}, {memory, PIDMem}, {main, self()},
                           {stdout, PIDio}]),
    {Globals, Labels} = Defines,
    NGlobals = linker(PIDM, Globals, Strings, Config),
    PIDCtrl = spawn(rvscorehw, control, [PIDM, PP, {NGlobals, Labels}, Data, 0]),
    TimeOutMain = maps:get(timeout_main, Config),
    receive
        ok ->
            logger:debug("main got ok - and finishes regularily"),
            ok
    after
        TimeOutMain ->
            logger:notice("timeout main"),
            timeout
    end,
    timer:sleep(50),
    Output = output_config(PIDM, Config),
    logger:info("Config: ~p", [Config]),
    timer:sleep(100),
    exit(PIDRegs, kill), exit(PIDMem, kill), exit(PIDwd, kill), exit(PIDio, kill),
    case is_process_alive(PIDCtrl) of
        true -> exit(PIDCtrl, kill);
        _ -> ok
    end,
    {{maps:put(main, self(), PIDM), PIDCtrl}, Output}.

%% -------------------------------------------------------------------
%% @doc
%% Converts a program to a list of strings.
%%
%% @spec program_to_strings(list()) -> list()
%% -------------------------------------------------------------------
program_to_strings(Program) ->
    lists:foldl(fun(Op, Acc) -> Acc ++ [op_to_string(Op)] end, [], Program).

%% -------------------------------------------------------------------
%% @doc
%% Converts an operation to a string representation.
%%
%% @spec op_to_string(tuple()) -> tuple()
%% -------------------------------------------------------------------
op_to_string(Operation) ->
    list_to_tuple(lists:foldl(fun(X, Acc) ->
                                  if 
                                      (is_number(X) or is_list(X)) ->
                                          Acc ++ [X]; 
                                      is_atom(X) ->
                                          Acc ++ [atom_to_list(X)];
                                      true ->
                                          Acc
                                  end
                              end, [], tuple_to_list(Operation))).

%% -------------------------------------------------------------------
%% @doc
%% Links the global variables and strings, and updates the system arguments.
%%
%% @spec linker(map(), map(), map(), map()) -> map()
%% -------------------------------------------------------------------
linker(PIDM, Globals, Strings, Config) ->
    case maps:find(input, Config) of
        {ok, Values} -> update_sysargs(PIDM, Values);
        error -> ok
    end,
    {_, NGlobs} = lists:foldl(fun(S, {I, GG}) ->
                maps:get(memory, PIDM) ! { self(), store, I,
                                          binary:list_to_bin(maps:get(S, Strings)) },
                case maps:find(S, GG) of
                    {ok, M} ->
                        N = maps:update(addr, I, M),
                        {I + length(maps:get(S, Strings)) + 1, maps:update(S, N, GG)};
                    _Default ->
                        {I + length(maps:get(S, Strings)) + 1, maps:put(S, #{addr => I}, GG)}
                end
            end, {10000, Globals}, maps:keys(Strings)),
    NGlobs.

%% -------------------------------------------------------------------
%% @doc
%% Updates the system arguments in memory.
%%
%% @spec update_sysargs(map(), list()) -> ok
%% -------------------------------------------------------------------
update_sysargs(PIDM, Values) ->
    if 
        is_list(Values) ->
            rvscorehw:save_memory(PIDM, 4000, length(Values)),
            F = fun(X, {I, LI}) -> 
                    case is_list(X) of
                        true ->
                            rvscorehw:save_memory(PIDM, 4004 + I, LI),
                            rvslibs:save_string(PIDM, X, LI),
                            {I + 4, LI + length(X) + 1};
                        _ ->
                            rvscorehw:save_memory(PIDM, 4004 + I, X),
                            {I + 4, LI}
                    end
                end,
            lists:foldl(F, {0, 4100}, Values);
        true ->
            throw({"input values not an array, usage {input, [1, 2, 3]}, but", Values})
    end.

%% -------------------------------------------------------------------
%% @doc
%% Outputs the configuration of registers and memory.
%%
%% @spec output_config(map(), map()) -> {list(), list(), integer()}
%% -------------------------------------------------------------------
output_config(PIDM, Config) ->
    Regs = case maps:find(dumpregs, Config) of
               {ok, _Range} ->
                   dump_registers(PIDM);
               _ ->
                   []
           end,
    Mem = case maps:find(dumpmemory, Config) of
              {ok, [A, B]} ->
                  dump_memory(PIDM, A, B);
              {ok, _R} ->
                  throw({"Usage {dumpmemory, [A, B]}: got dumpmemory,", _R});
              _ ->
                  []
          end,
    case maps:find(dumpoutput, Config) of
        {ok, [LA, LB]} ->
            LMem = dump_memory(PIDM, LA, LB),
            logger:notice("dumpoutput: ~s", [LMem]),
            maps:get(stdout, PIDM) ! {print, LMem};
        {ok, _Reason} ->
            throw({"Usage {dumpoutput, [A, B]}: got: dumpoutput ,", _Reason});
        _ ->
            ok
    end,    
    RetVal = rvscorehw:load_register(PIDM, "a0"),
    {Regs, Mem, RetVal}.

%% -------------------------------------------------------------------
%% @doc
%% Dumps the current state of the registers.
%%
%% @spec dump_registers(map()) -> list()
%% -------------------------------------------------------------------
dump_registers(PIDM) ->
    maps:get(registers, PIDM) ! { self(), dump },
    TimeOutDump = 100,
    receive
        {ok, Registers} ->
            Registers,
            lists:sort(fun({A, _}, {B, _}) -> A < B end, Registers)
    after
        TimeOutDump ->
            logger:error("timeout dump_registers"),
            timeout
    end.

%% -------------------------------------------------------------------
%% @doc
%% Dumps the memory content from address A to E.
%%
%% @spec dump_memory(map(), integer(), integer()) -> binary() | timeout
%% -------------------------------------------------------------------
dump_memory(PIDM, A, E) ->
    maps:get(memory, PIDM) ! { self(), dump },
    TimeOutDump = 100,
    Memory = receive
                 {ok, Mem} ->
                     Mem
             after
                 TimeOutDump ->
                     logger:error("timeout dump_memory"),
                     timeout
             end,
    case Memory of
        timeout -> timeout;
        _ -> 
            LMem = lists:foldl(fun(V, Acc) ->
                                Acc ++ [binary:at(Memory, V)]
                            end, [], lists:seq(A, E)),
            binary:list_to_bin(LMem)
    end.

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------------------------------
%% @doc
%% Tests dumping registers and handling memory dump timeout.
%%
%% @spec rvsmain_dump_register_and_timeout_dump_memory_test() -> ok
%% -------------------------------------------------------------------
rvsmain_dump_register_and_timeout_dump_memory_test() ->
    PIDRegs = spawn(rvscorehw, registers, [init, 32, 17]),
    Regs = dump_registers(maps:from_list([{registers, PIDRegs}])),
    ?assertEqual(17, maps:get("a15", maps:from_list(Regs))),
    kill([PIDRegs]),
    ?assertEqual(timeout, dump_registers(maps:from_list([{registers, PIDRegs}]))),
    ?assertEqual(timeout, dump_memory(maps:from_list([{memory, PIDRegs}]), 400, 404)).

%% -------------------------------------------------------------------
%% @doc
%% Tests killing the control process.
%%
%% @spec rvsmain_kill_control_test() -> ok
%% -------------------------------------------------------------------
rvsmain_kill_control_test() ->
    {_, _} = run(),
    timer:sleep(1000),
    %%kill(PIDCtrl).    
    ok.

%% -------------------------------------------------------------------
%% @doc
%% Tests converting operations to string.
%%
%% @spec rvsmain_op_to_string_test() -> ok
%% -------------------------------------------------------------------
rvsmain_op_to_string_test() ->
    Result = op_to_string({"saf", asd, 1, digraph:new()}),
    ?assertEqual(3, tuple_size(Result)),
    ok.
-endif.
