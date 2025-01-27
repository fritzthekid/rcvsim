%% -------------------------------------------------------------------
%% @doc
%% This module provides functions to read and process RISC-V assembly files.
%% It includes functionalities to remove comments, split labels and code,
%% extract global variables, and handle different properties of globals.
%% -------------------------------------------------------------------

-module(rvsreadasm).
-compile(export_all).

%% -------------------------------------------------------------------
%% @doc
%% Reads an assembly file, processes it to remove comments, extract globals,
%% split labels and code, and grep strings.
%%
%% @spec readasm(Filename :: string()) -> {list(), {map(), map()}, map()}.
%% -------------------------------------------------------------------
readasm(Filename) ->
    Text = remove_comment(read_text_file_as_list(Filename)),
    rvsutils:write_terms("bck/rawprog_as_list.s", Text),
    Globals = globals_maps(Text),
    {_, Labs, Code} = split_labels_code(Text),
    {Code, {Globals, maps:from_list(Labs)}, grep_strings(Text)}.

%% -------------------------------------------------------------------
%% @doc
%% Reads a text file and returns its content as a list of strings.
%%
%% @spec read_text_file_as_list(Filename :: string()) -> list().
%% -------------------------------------------------------------------
read_text_file_as_list(Filename) ->
    [_|Text] = case file:read_file(Filename) of
                   {ok, Bin} -> 
                       string:split(binary:bin_to_list(Bin), "\n", all);
                   {error, enoent} ->
                       logger:error("File not found, ~p", [Filename]),
                       [error, error];
                   {error, Reason} ->
                       logger:error("Error: ~p, ~p", [Reason, Filename]),
                       [error, error]
               end,
    Text.

%% -------------------------------------------------------------------
%% @doc
%% Splits the text into labels and code.
%%
%% @spec split_labels_code(Text :: list()) -> {integer(), list(), list()}.
%% -------------------------------------------------------------------
split_labels_code(Text) ->
    lists:foldl(fun split_line_labels_code/2, {0, [], []}, Text).

split_line_labels_code(L, {I, Labs, Code}) ->
    IsLabel = re:run(L, "^[\.a-zA-Z][\.a-zA-Z0-9_]+:"),
    IsCode = re:run(L, "^\t[a-z].*"),
    IsDirective = re:run(L, "^\t[\.].*"),
    case {IsLabel, IsCode, IsDirective} of
        {nomatch, nomatch, {match, _}} ->
            {I, Labs, Code};
        {nomatch, {match, _}, nomatch} ->
            {I + 1, Labs, Code ++ [{I, do_line(L)}]};
        {{match, _}, nomatch, nomatch} ->
            {I, Labs ++ [{hd(string:split(L, ":")), I}], Code};
        _Default ->
            logger:info("line no directive or code or comment: ~p", [L]),
            {I, Labs, Code}
    end.

%% -------------------------------------------------------------------
%% @doc
%% Greps strings from the text.
%%
%% @spec grep_strings(Text :: list()) -> map().
%% -------------------------------------------------------------------
grep_strings(Text) ->
    {_, LL} = lists:foldl(fun(Line, {I, Acc}) ->
                             case re:run(Line, "[a-zA-Z_]+:") of
                                 {match, [{_, E}]} ->
                                     {I + 1, Acc ++ [{lists:sublist(Line, E - 1), I}]};
                                 _Default ->
                                     {I + 1, Acc}
                             end
                         end, {1, []}, Text),
    LM = maps:from_list(LL),
    LS = lists:foldl(fun(Lab, Acc) -> 
                         L = hd(lists:sublist(Text, maps:get(Lab, LM) + 1, 1)),
                         case re:run(L, "\.string\t") of
                             {match, [{_, E}]} ->
                                 SL = lists:sublist(L, E + 3, length(L) - E - 3),
                                 Acc ++ [{Lab, SL}];
                             _Default ->
                                 Acc
                         end
                     end, [], maps:keys(LM)),
    maps:from_list(LS).

%% -------------------------------------------------------------------
%% @doc
%% Removes comments from the text.
%%
%% @spec remove_comment(Text :: list()) -> list().
%% -------------------------------------------------------------------
remove_comment(Text) ->
    [H | T] = Text,
    lists:foldl(fun(L, Acc) ->
                    case re:run(L, "^[ ]*#.*") of
                        {match, _} ->
                            Acc;
                        nomatch ->
                            Acc ++ [L]
                    end
                end, [H], T).

%% -------------------------------------------------------------------
%% @doc
%% Extracts global variables and their properties from the text.
%%
%% @spec globals_maps(Text :: list()) -> map().
%% -------------------------------------------------------------------
globals_maps(Text) ->
    G = sets:to_list(sets:from_list(grep_globals(Text))),
    SM = maps:from_list(prop_of_globals(Text, G, "size")),
    TM = maps:from_list(prop_of_globals(Text, G, "type")),
    AM = maps:from_list(prop_of_globals(Text, G, "addr")),
    GM = maps:from_list(lists:foldl(fun(SG, Acc) ->
                                       Acc ++ [{SG, maps:from_list(
                                               [{size, maps:get(SG, SM, "0")},
                                                {type, maps:get(SG, TM, "_")},
                                                {addr, maps:get(SG, AM, "-1")}]
                                           )}]
                                   end, [], G)),
    GM.

append_key_values(M, []) -> M;
append_key_values(M, [{G, K, V} | T]) ->
    append_key_values(append_key_values(M, G, K, V), T).

append_key_values(M1, G, K, V) ->
    case maps:find(G, M1) of
        {ok, _} ->
            maps:update(G, maps:merge(maps:get(G, M1), #{K => V}), M1);
        error ->
            maps:put(G, #{K => V}, M1)
    end.

%% -------------------------------------------------------------------
%% @doc
%% Prints the text with line numbers.
%%
%% @spec print_text(Text :: list()) -> ok.
%% -------------------------------------------------------------------
print_text(Text) ->
    lists:foreach(fun({I, L}) -> io:format("~p: ~s~n", [I, L]) end, lists:zip(lists:seq(1, length(Text)), Text)).

%% -------------------------------------------------------------------
%% @doc
%% Returns a sublist of the text based on the provided range.
%%
%% @spec list_text(Text :: list(), A :: integer(), E :: integer()) -> list().
%% -------------------------------------------------------------------
list_text(Text, A, E) ->
    lists:foldl(fun({I, L}, Acc) -> IsIn = ((I >= A) and (I < E)), 
                                  if 
                                      IsIn -> 
                                          Acc ++ [L]; 
                                      true -> 
                                          Acc 
                                  end
                end, [], lists:zip(lists:seq(1, length(Text)), Text)).

%% -------------------------------------------------------------------
%% @doc
%% Processes a single line of code.
%%
%% @spec do_line(L :: string()) -> list().
%% -------------------------------------------------------------------
do_line(L) ->
    case hd(L) of
        9 ->
            [_ | T] = L;
        _ ->
            T = L
    end,
    case re:run(T, "\t") of
        nomatch ->
            do_clean_line(string:strip(T));
        _ ->
            do_clean_line(string:strip(string:replace(T, "\t", " ", all)))
    end.

do_clean_line(L) ->
    logger:debug("clean line: ~p", [L]),
    LL = string:split(L, ",", all),
    LLL = lists:foldl(fun(S, Acc) -> Acc ++ string:split(S, " ") end, [], LL),
    LLLL = lists:foldl(fun(S, Acc) -> Acc ++ [string:strip(S)] end, [], LLL),
    F = fun(S, Acc) ->
            case string:to_integer(S) of
                {error, _} -> Acc ++ [S];
                {Int, []} -> Acc ++ [Int];
                {_, _} -> Acc ++ [S]
            end
        end,
    lists:foldl(F, [], LLLL).

%% -------------------------------------------------------------------
%% @doc
%% Greps global variables from the text.
%%
%% @spec grep_globals(Text :: list()) -> list().
%% -------------------------------------------------------------------
grep_globals(Text) ->
    lists:foldl(fun(Line, Acc) ->
                    case {re:run(Line, "\.globl"), re:run(Line, "\.type")} of
                        {nomatch, nomatch} ->
                            Acc;
                        {{match, [{Le, Len}]}, nomatch} ->
                            Acc ++ [string:strip(lists:sublist(Line, Le + Len + 2, 80))];
                        {nomatch, {match, [{LLe, LLen}]}} ->
                            case re:run(lists:sublist(Line, LLe + LLen + 2, 80), ".*,") of
                                nomatch -> Acc;
                                {match, [{_, LR}]} ->
                                    L = lists:sublist(string:strip(
                                                      lists:sublist(Line, 8, 80)), 1, LR - 1),
                                    Acc ++ [L];
                                _Default ->
                                    Acc ++ [fritzerror]
                            end;
                        _Default ->
                            Acc ++ [error]
                    end
                end, [], Text).

%% -------------------------------------------------------------------
%% @doc
%% Extracts properties of global variables from the text.
%%
%% @spec prop_of_globals(Text :: list(), Globals :: list(), Prop :: string()) -> list().
%% -------------------------------------------------------------------
prop_of_globals(Text, Globals, Prop) ->
    prop_of_globals([], Globals, Text, Prop).

prop_of_globals(Acc, [], _, _) -> Acc;
prop_of_globals(OAcc, [G | T], Text, Prop) ->
    NAcc = lists:foldl(fun(Line, Acc) ->
                           case re:split(Line, "[\t \,]", [{return, list}]) of
                               [[],"." ++ Prop, G, [], Num] ->
                                   Acc ++ [{G, Num}];
                               _Default ->
                                   Acc
                           end
                       end, OAcc, Text),
    prop_of_globals(NAcc, T, Text, Prop).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------------------------------
%% @doc
%% Test for list_text function.
%% -------------------------------------------------------------------
list_text_test() ->
    Text = read_text_file_as_list("_build/obj/test1.s"),
    print_text(Text).

%% -------------------------------------------------------------------
%% @doc
%% Test for read_text_file function.
%% -------------------------------------------------------------------
read_text_file_test() ->
    [error] = read_text_file_as_list("test/x/somefile").

%% -------------------------------------------------------------------
%% @doc
%% Test for do_line function.
%% -------------------------------------------------------------------
do_line_test() ->
    ?assertEqual(["nop"], do_line("\tnop")),
    ?assertEqual(["nop"], do_line("nop")),
    ?assertEqual(["nop", "nop"], do_line("\tnop   nop")),
    ?assertEqual(["nop", "nop"], do_line("\tnop\t   nop")),
    ok.

-endif.
