-module(rvsdis).
-compile(export_all).
%%-export([new/2,numtobin/2,bintonum/2,putdata/4,put/4,getdata/3,get/3]).

h2i(arg) ->
    list_to_integer(arg).

opcode(instruction) ->
    opcode band h2i("7f").

format(instruction) ->
    case opcode(instruction) of
	%% "37" -> fU;
	%% h2i("17") -> fU;
	%% h2i("6f") -> fJ;
	%% h2i("67") -> fI;
	%% h2i("63") -> fB;
	%% h2i("03") -> fI;
	%% h2i("23") -> fS;
	%% h2i("13") -> fI;
	%% h2i("33") -> fR;
	%% h2i("0f") -> fI;
	%% h2i("73") -> fI;
	_ -> throw("unknown instruction format")
    end.

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
