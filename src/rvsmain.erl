-module(rvsmain).
-compile([export_all]).

kill(PIDL) ->
    lists:filter(fun(X) -> X ! kill, true end, PIDL).

do() ->
    ROOT=".",
    {ok, [Config]}  = file:consult(ROOT ++ "/data/config.config"),
    io:format("Config: ~w~n",[Config]),
    {ok, [P]} = file:consult(ROOT ++ "/data/" ++ maps:get(programname,Config)),
    %% io:format("Program:~n",[]),
    lists:foldl(fun(B,Acc) ->
			 io:format("~p: ~p~n",[Acc,B]),
			 Acc+1
		 end, 0, P),
    PP = element(2,lists:foldl(fun(X,{I,Acc}) -> {I+1, Acc++[{I,tuple_to_list(X)}]} end, {0,[]}, P)),
    {ok, [Data]} = file:consult(ROOT ++ "/data/" ++ maps:get(dataname,Config)),
    io:format("Data:~n",[]),
    lists:foldl(fun(B,Acc) ->
			 io:format("~p: ~p~n",[Acc,B]),
			 Acc+1
		end, 0, Data),
    NumReg = maps:get(registers,Config),
    InitialRegs = lists:foldl(fun(X,Acc) -> Acc++[{X,0}] end, [], lists:seq(1,NumReg)),
    PIDRegs = spawn(rvscorehw, registers, [InitialRegs]),
    %io:format("rsvmain: ~p, program, ~p~n",[0,PP]),
    PIDCtrl = spawn(rvscorehw, control, [PIDRegs, PP, Data, 0]),
    PIDL = [PIDRegs,PIDCtrl],
    %%lists:foldl(fun({OP}
    PIDL.


-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
rcvmain_do_test() ->
    PIDL=do(),
    timer:sleep(1000),
    kill(PIDL).
-endif.
