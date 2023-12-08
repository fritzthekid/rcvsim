-module(rcvmain).
-compile([export_all]).

do() ->
    ROOT=".",
    {ok, [Config]}  = file:consult(ROOT ++ "/data/config.cfg"),
    io:format("Config: ~w~n",[Config]),
    {ok, [Program]} = file:consult(ROOT ++ "/data/" ++ maps:get(programname,Config)),
    io:format("Program:~n",[]),
    lists:foldl(fun(B,Acc) ->
			 io:format("~p: ~p~n",[Acc,B]),
			 Acc+1
		 end, 0, Program),
    {ok, [Data]} = file:consult(ROOT ++ "/data/" ++ maps:get(dataname,Config)),
    io:format("Data:~n",[]),
    lists:foldl(fun(B,Acc) ->
			 io:format("~p: ~p~n",[Acc,B]),
			 Acc+1
		end, 0, Data),
    ok.


-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
rcvmain_do_test() ->
    ok=do().
-endif.
