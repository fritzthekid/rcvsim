-module(rvsmain).
-compile([export_all]).

kill(PIDL) ->
    lists:filter(fun(x) -> X ! kill, true end, PIDL).

do() ->
    ROOT=".",
    {ok, [Config]}  = file:consult(ROOT ++ "/data/config.cfg"),
    io:format("Config: ~w~n",[Config]),
    {ok, [P]} = file:consult(ROOT ++ "/data/" ++ maps:get(programname,Config)),
    %% io:format("Program:~n",[]),
    lists:foldl(fun(B,Acc) ->
			 io:format("~p: ~p~n",[Acc,B]),
			 Acc+1
		 end, 0, Program),
    PP = foldl(fun(X,{J, Acc}) = [Acc]++[{J,tuple_to_list(X)}] end, {0,[]}, P),
    {ok, [Data]} = file:consult(ROOT ++ "/data/" ++ maps:get(dataname,Config)),
    io:format("Data:~n",[]),
    lists:foldl(fun(B,Acc) ->
			 io:format("~p: ~p~n",[Acc,B]),
			 Acc+1
		end, 0, Data),
    NumReg = maps:get(register,Config),
    InitialRegs = lists:foldr(fun(_,Acc) -> [0] ++ Acc end, [], lists:seq(1,NumReg)),
    PIDRegs = spawn(rvscorehw, registers, [InitialRegs]),
    PIDCtrl = spawn(rvscorehw, control, [PIDRegs, Program, Data, 0]),
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
