%% -------------------------------------------------------------------
%% @doc
%% This module contains the standard library functions for the RISC-V
%% Simulator. It includes functions for string manipulation, message
%% sending, and other utility functions used during simulation.
%% -------------------------------------------------------------------

-module(rvslibs).
-compile([export_all]).

%% -------------------------------------------------------------------
%% @doc
%% This module provides standard library functions for the RISC-V Simulator.
%% It includes functions for string manipulation and message sending.
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc
%% Calls the appropriate external function based on the operation.
%%
%% @spec stdlib(map(), list(), map(), integer()) -> ok
%% -------------------------------------------------------------------
stdlib(PIDM, Op, Defines, PC) ->
    case lists:last(Op) of
        "rvs_strtol_extern" ->
            stdlib_strtol(PIDM, Op, Defines, PC);
        "rvs_sendmessage_extern" ->
            rvslib_sendmessage(PIDM, Op, Defines, PC);
        _R ->
            logger:notice("stdlib, ~p not defined", [_R])
    end.

%% -------------------------------------------------------------------
%% @doc
%% Saves a string value to memory at the specified location.
%%
%% @spec save_string(map(), list(), integer()) -> ok
%% -------------------------------------------------------------------
save_string(PIDM, Val, Loc) ->
    logger:debug("rvsmain,save_string: ~p, ~s", [Loc, Val]),
    F = fun(X, I) ->
            rvscorehw:save_memory(PIDM, I, X, char),
            I + 1
        end,
    lists:foldl(F, Loc, Val),
    rvscorehw:save_memory(PIDM, Loc + length(Val), 0, char).

%% -------------------------------------------------------------------
%% @doc
%% Loads a string value from memory starting at the specified location.
%%
%% @spec load_string(map(), integer()) -> list()
%% -------------------------------------------------------------------
load_string(PIDM, Loc) ->
    load_string(PIDM, Loc, [], 0).

%% -------------------------------------------------------------------
%% @doc
%% Helper function to load a string value from memory with depth control.
%%
%% @spec load_string(map(), integer(), list(), integer()) -> list()
%% -------------------------------------------------------------------
load_string(PIDM, Loc, Acc, Depth) ->
    C = rvscorehw:load_memory(PIDM, Loc, char),
    if 
        Depth > 100 ->
            throw("rvslibs,load_string: String detection fails: Depth > 100");
        C =:= 0 ->
            Acc;
        true ->
            load_string(PIDM, Loc + 1, Acc ++ [C], Depth + 1)
    end.

%% -------------------------------------------------------------------
%% @doc
%% Converts a string to an integer using the strtol function.
%%
%% @spec stdlib_strtol(map(), list(), map(), integer()) -> ok
%% -------------------------------------------------------------------
stdlib_strtol(PIDM, _Op, _Defines, _PC) ->
    Ptr = rvscorehw:load_register(PIDM, "a0"),
    Str = load_string(PIDM, Ptr),
    Val = case string:to_integer(Str) of
        {V, []} ->
            V;
        _R ->
            throw({"rvslibs,strlib_strtol string not an integer", Str})
    end,
    rvscorehw:save_register(PIDM, "a0", Val),
    ok.

%% -------------------------------------------------------------------
%% @doc
%% Sends a message to the standard output.
%%
%% @spec rvslib_sendmessage(map(), list(), map(), integer()) -> ok
%% -------------------------------------------------------------------
rvslib_sendmessage(PIDM, _Op, _Defines, _PC) ->
    Ptr = rvscorehw:load_register(PIDM, "a0"),
    Str = load_string(PIDM, Ptr),
    maps:get(stdout, PIDM) ! { print, Str },
    ok.

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------------------------------
%% @doc
%% Tests the strtol function.
%%
%% @spec strtol_test() -> ok
%% -------------------------------------------------------------------
strtol_test() ->
    PIDMem = spawn(rvsmemory, memory, [init, 40]),
    PIDReg = spawn(rvscorehw, registers, [init, 32, 0]),
    PIDM = #{registers => PIDReg, memory => PIDMem},
    rvscorehw:save_register(PIDM, "a0", 0),
    lists:foldl(fun(C, I) ->
                    rvscorehw:save_memory(PIDM, I, C, char),
                    I + 1
                end, 0, "4711"),
    PIDMem ! {self(), dump},
    receive
        {ok, _Memory} ->
            ok
    end,
    stdlib_strtol(PIDM, [], #{}, 0),
    Val = rvscorehw:load_register(PIDM, "a0"),
    PIDMem ! kill,
    ?assertEqual(4711, Val).
-endif.
