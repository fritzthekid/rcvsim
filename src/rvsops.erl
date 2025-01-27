%% -------------------------------------------------------------------
%% @doc
%% This module provides functions for disassembling RISC-V instructions.
%% The actual disassembly logic is implemented in a NIF (Native Implemented Function) library.
%% -------------------------------------------------------------------

-module(rvsops).
-export([disassemble/1, disassembleraw/1]).
-nifs([disassemble/1, disassembleraw/1]).
-on_load(init/0).

%% -------------------------------------------------------------------
%% @doc
%% Initializes the NIF library.
%%
%% @spec init() -> ok | {error, term()}
%% -------------------------------------------------------------------
init() ->
    erlang:load_nif("./_build/libs/rvsops", 0).

%% -------------------------------------------------------------------
%% @doc
%% Disassembles a RISC-V instruction.
%%
%% @spec disassemble(binary()) -> list() | no_return()
%% -------------------------------------------------------------------
disassemble(_X) ->
    erlang:nif_error("NIF library not loaded").

%% -------------------------------------------------------------------
%% @doc
%% Disassembles a raw RISC-V instruction.
%%
%% @spec disassembleraw(binary()) -> list() | no_return()
%% -------------------------------------------------------------------
disassembleraw(_X) ->
    erlang:nif_error("NIF library not loaded").

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------------------------------
%% @doc
%% Tests the disassemble function with an ADDI instruction.
%%
%% @spec addi_test() -> ok
%% -------------------------------------------------------------------
addi_test() ->
    ?assertEqual(["addi", "x14", "x0", "8"], disassemble(list_to_integer("00800713", 16))),
    ok.

%% -------------------------------------------------------------------
%% @doc
%% Tests the disassemble function with a JAL instruction.
%%
%% @spec jal_test() -> ok
%% -------------------------------------------------------------------
jal_test() ->
    ?assertEqual(["jal", "x1", "4"], disassemble(list_to_integer("004000ef", 16))),
    ok.

%% -------------------------------------------------------------------
%% @doc
%% Tests the disassemble function with an invalid instruction to ensure it raises an error.
%%
%% @spec error_test() -> ok
%% -------------------------------------------------------------------
error_test() ->
    try
        case disassemble(0) of
            _ -> ?assert(false)
        end
    catch 
        error:Reason -> ?assertEqual(badarg, Reason)
    end,
    ok.
-endif.
