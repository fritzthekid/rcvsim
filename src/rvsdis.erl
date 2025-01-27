%% -------------------------------------------------------------------
%% @doc
%% This module provides functionalities to disassemble binary RISC-V
%% instructions. It includes reading binary files, disassembling them,
%% and converting them to human-readable formats.
%% -------------------------------------------------------------------

-module(rvsdis).
-compile(export_all).

%% -------------------------------------------------------------------
%% @doc
%% This module provides functionalities to disassemble binary RISC-V
%% instructions. It includes reading binary files, disassembling them,
%% and converting them to human-readable formats.
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% @doc
%% Reads the content of a binary file.
%%
%% @spec read_binary(string()) -> binary()
%% -------------------------------------------------------------------
read_binary(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Bin.

%% -------------------------------------------------------------------
%% @doc
%% Attempts to disassemble a word and returns the result.
%%
%% @spec try_disassemble(integer(), binary()) -> list() | error
%% -------------------------------------------------------------------
try_disassemble(I, Word) ->
    try
        %%logger:info("try disassemble Word: ~p:~p~n",[I,Word]),
        case rvsops:disassembleraw(binary:decode_unsigned(Word)) of
            Res ->
                io:format("~p: ~p~n", [integer_to_list(I, 16), Res]),
                [integer_to_list(I, 16), Res]
        end
    catch error:_R -> 
        logger:info("warning: ~p: ~p ~p~n", [integer_to_list(I, 16), Word, _R]),
        error
    end.

%% -------------------------------------------------------------------
%% @doc
%% Analyzes the binary content of a file and disassembles it.
%%
%% @spec analyse(string()) -> list()
%% -------------------------------------------------------------------
analyse(Filename) ->
    Bin = read_binary(Filename),
    analyse(Bin, 0, byte_size(Bin) - 4 - 1).

%% -------------------------------------------------------------------
%% @doc
%% Analyzes a binary by iterating from start to end positions.
%%
%% @spec analyse(binary(), integer(), integer()) -> list()
%% -------------------------------------------------------------------
analyse(Bin, Start, End) ->
    Count = byte_size(Bin) - 4,
    io:format("byte_size(Bin): ~p ~p~n", [byte_size(Bin), Count]),
    lists:foldl(fun(X, Acc) ->
                    Word = rvsda:get(Bin, X, 4),
                    RevWord = binary:list_to_bin(lists:reverse(binary:bin_to_list(Word))),
                    case try_disassemble(X, RevWord) of
                        error -> Acc;
                        Res -> Acc ++ [Res]
                    end
                end, [], lists:seq(Start, End, 4)).

%% -------------------------------------------------------------------
%% @doc
%% Extracts instructions from the program.
%%
%% @spec instractutions(list()) -> list()
%% -------------------------------------------------------------------
instractutions(Prog) ->
    lists:foldl(fun(X, Acc) -> Acc ++ tl(X) end, [], Prog).

%% -------------------------------------------------------------------
%% @doc
%% Returns the name of the register corresponding to a value.
%%
%% @spec registername(integer()) -> string()
%% -------------------------------------------------------------------
registername(V) ->
    case V of
        0 -> "zero";
        1 -> "ra";
        2 -> "sp";
        3 -> "gp";
        4 -> "tp";
        5 -> "t0";
        6 -> "t1";
        7 -> "t2";
        8 -> "s0";
        9 -> "s1";
        10 -> "a0";
        11 -> "a1";
        12 -> "a2";
        13 -> "a3";
        14 -> "a4";
        15 -> "a5";
        16 -> "a6";
        17 -> "a7";
        18 -> "s2";
        19 -> "s3";
        20 -> "s4";
        21 -> "s5";
        22 -> "s6";
        23 -> "s7";
        24 -> "s8";
        25 -> "s9";
        26 -> "s10";
        27 -> "s11";
        28 -> "t3";
        29 -> "t4";
        30 -> "t5";
        31 -> "t6";
        _R -> error
    end.
        
%% -------------------------------------------------------------------
%% @doc
%% Reads the code operator based on the provided values.
%%
%% @spec readcodeoperator(list()) -> string() | integer()
%% -------------------------------------------------------------------
readcodeoperator([C, V]) ->
    if 
        C == 0 -> 
            V;
        true ->
            registername(V)
    end.

%% -------------------------------------------------------------------
%% @doc
%% Converts a program into a readable format.
%%
%% @spec readable(list()) -> list()
%% -------------------------------------------------------------------
readable(Prog) ->
    lists:foldl(fun([Addr, Inst], Acc) ->
                    case length(Inst) of
                        4 ->
                            [I, OP0, OP1, OP2] = Inst,
                            Acc ++ [[Addr, [I, 
                                readcodeoperator(OP0),
                                readcodeoperator(OP1),
                                readcodeoperator(OP2)]]];
                        3 ->
                            [I, OP0, OP1] = Inst,
                            Acc ++ [[Addr, [I, 
                                readcodeoperator(OP0),
                                readcodeoperator(OP1)]]];
                        2 ->
                            [I, OP0] = Inst,
                            Acc ++ [[Addr, [I, 
                                readcodeoperator(OP0)]]];
                        _R ->
                            Acc ++ [[Addr, (null)]]
                    end
                end, [], Prog).

%% -------------------------------------------------------------------
%% @doc
%% Prints the readable format of the program.
%%
%% @spec printreadable(list()) -> ok
%% -------------------------------------------------------------------
printreadable(Prog) ->
    lists:foreach(fun([Addr, Inst]) -> 
                      io:format("~p: ~p~n", [Addr, Inst]) 
                  end, readable(Prog)).

%% -------------------------------------------------------------------
%% @doc
%% Converts a binary to hexadecimal representation.
%%
%% @spec bin_to_hex(binary()) -> string()
%% -------------------------------------------------------------------
bin_to_hex(Bin) ->
    list_to_hex(binary:bin_to_list(Bin)).

%% -------------------------------------------------------------------
%% @doc
%% Converts a list of integers to a hexadecimal string.
%%
%% @spec list_to_hex(list()) -> string()
%% -------------------------------------------------------------------
list_to_hex(L) ->
    list_to_hex(L, "0x").

%% -------------------------------------------------------------------
%% @doc
%% Helper function to convert a list of integers to a hexadecimal string.
%%
%% @spec list_to_hex(list(), string()) -> string()
%% -------------------------------------------------------------------
list_to_hex([], R) -> R;
list_to_hex(L, R) ->
    [H | TL] = L,
    NR = R ++ integer_to_list(H, 16),
    list_to_hex(TL, NR).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------------------------------
%% @doc
%% Tests the analyse function.
%%
%% @spec analyse_test() -> ok
%% -------------------------------------------------------------------
analyse_test() ->
    P = analyse("_build/obj/test2.o"),
    ?assert(length(P) > 10),
    ok.
-endif.
