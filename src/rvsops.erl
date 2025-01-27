-module(rvsops).

-export([disassemble/1, disassembleraw/1]).

-nifs([disassemble/1, disassembleraw/1]).

-on_load(init/0).

%% Initialize the NIF library
init() ->
    erlang:load_nif("./_build/libs/rvsops", 0).

%% Fallback implementation if the NIF library is not loaded
disassemble(_X) ->
    erlang:nif_error("NIF library not loaded").

%% Fallback implementation if the NIF library is not loaded
disassembleraw(_X) ->
    erlang:nif_error("NIF library not loaded").

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").

%% Test for the addi instruction
addi_test() ->
    ?assertEqual(["addi", "x14", "x0", "8"], disassemble(list_to_integer("00800713", 16))),
    ok.

%% Test for the jal instruction
jal_test() ->
    ?assertEqual(["jal", "x1", "4"], disassemble(list_to_integer("004000ef", 16))),
    ok.

%% Test for error handling
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
