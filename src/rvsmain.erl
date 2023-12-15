-module(rvsmain).
-compile([export_all]).

kill(PIDL) ->
    lists:foreach(fun(X) -> X ! kill end, PIDL).

do() ->
    do("/data/config.config").
do(Configfilename) ->
    ROOT=".",
    {ok, [Config]}  = file:consult(ROOT ++ Configfilename),
    [_|P] = rvsreadasm:readasm(ROOT ++ "/data/simple-func.s"),
    PP = element(2,lists:foldl(fun(X,{I,Acc}) -> 
				       {I+1, Acc++[{I,tuple_to_list(X)}]} 
			       end, {0,[]}, program_to_strings(P))),
    {ok, [Data]} = file:consult(ROOT ++ "/data/" ++ maps:get(dataname,Config)),
    {ok, [OTL]} = file:consult(ROOT ++ "/src/operation-table.config"),
    OpTab = dict:from_list(OTL),
    PIDRegs = spawn(rvscorehw, registers, [init,maps:get(registers,Config),0]),
    PIDMem =  spawn(rvscorehw, registers, [init,maps:get(memory,Config),0]),
    PIDM = maps:from_list([{registers,PIDRegs},{memory,PIDMem},{main,self()}]),
    PIDCtrl = spawn(rvscorehw, control, [PIDM, PP, OpTab, Data, 0]),
    %%maps:fold(fun(_,V,Acc)->[V]++Acc end,[],PIDM)++[PIDCtrl].
    TimeOutMain = maps:get(timeout_main,Config),
    receive
	ok ->
	    ok
    after
	TimeOutMain ->
	    timeout
    end,
    {maps:put(main,self(),PIDM),PIDCtrl}.
    
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
rvsmain_kill_control_test() ->
    {_,_} = do(),
    timer:sleep(1000),
    %%kill(PIDCtrl).	
    ok.
rvsmain_op_to_string_test() ->
    Result = op_to_string({"saf",asd,1,digraph:new()}),
    ?assertEqual(3,tuple_size(Result)),
    ok.
-endif.
