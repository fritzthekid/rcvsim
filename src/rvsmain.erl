-module(rvsmain).
-compile([export_all]).

kill(PIDL) ->
    lists:filter(fun(X) -> X ! kill, true end, PIDL).

do() ->
    ROOT=".",
    {ok, [Config]}  = file:consult(ROOT ++ "/data/config.config"),
    %% io:format("Config: ~w~n",[Config]),
    %% {ok, [P]} = file:consult(ROOT ++ "/data/" ++ maps:get(programname,Config)),
    [_|P] = rvsreadasm:readasm(ROOT ++ "/data/simple-func.s"),
    PP = element(2,lists:foldl(fun(X,{I,Acc}) -> 
				       {I+1, Acc++[{I,tuple_to_list(X)}]} 
			       end, {0,[]}, program_to_strings(P))),
    {ok, [Data]} = file:consult(ROOT ++ "/data/" ++ maps:get(dataname,Config)),
    {ok, [OTL]} = file:consult(ROOT ++ "/src/operation-table.config"),
    OpTab = dict:from_list(OTL),
    %%io:format("Data:~n",[]),
    %%lists:foldl(fun(B,Acc) ->
		%% 	 io:format("~p: ~p~n",[Acc,B]),
		%% 	 Acc+1
		%% end, 0, Data),
    NumReg = maps:get(registers,Config),
    InitialRegs = lists:foldl(fun(X,Acc) -> 
				      Acc++[{"a"++integer_to_list(X),0}] end,
			      [], lists:seq(1,NumReg))++[{"sp",0},{"s0",0}],
    PIDRegs = spawn(rvscorehw, registers, [InitialRegs]),
    %io:format("rsvmain: ~p, program, ~p~n",[0,PP]),
    PIDCtrl = spawn(rvscorehw, control, [PIDRegs, PP, OpTab, Data, 0]),
    PIDL = [PIDRegs,PIDCtrl],
    %%lists:foldl(fun({OP}
    PIDL.

program_to_strings(Program) ->
    lists:foldl(fun(Op, Acc) -> Acc ++ [op_to_string(Op)] end, [], Program).

op_to_string(Operation) ->
    list_to_tuple(lists:foldl(fun(X,Acc)->
				      if 
					  (is_number(X) or is_list(X)) ->
					      Acc++[X]; 
					  is_atom(X) ->
					      Acc++[atom_to_list(X)];
					  true ->
					      Acc
				      end
			      end, [], tuple_to_list(Operation))).
							     

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
rcvmain_do_test() ->
    PIDL=do(),
    timer:sleep(1000),
    kill(PIDL).
-endif.
