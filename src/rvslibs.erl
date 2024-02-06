-module(rvslibs).
-compile([export_all]).

stdlib(PIDM,Op,Defines,PC) ->
    case lists:last(Op) of
	"rvs_strtol_extern" ->
	    stdlib_strtol(PIDM,Op,Defines,PC);
	_R ->
	    logger:notice("stdlib, ~p not defined",[_R])
    end.

save_string(PIDM,Val,Loc) ->
    logger:debug("rvsmain,save_string: ~p, ~s",[Loc,Val]),
    F = fun(X,I) ->
		rvscorehw:save_memory(PIDM,I,X,char),
		I+1
	end,
    lists:foldl(F,Loc,Val),
    rvscorehw:save_memory(PIDM,Loc+length(Val),0).

load_string(PIDM,Loc) ->
    load_string(PIDM,Loc,[],0).
load_string(PIDM,Loc,Acc,Depth) ->
    C = rvscorehw:load_memory(PIDM,Loc,char),
    if 
	Depth > 100 ->
	    throw("rvslibs,load_string: String detection fails: Depth > 100");
	C =:= 0 ->
	    Acc;
	true ->
	    load_string(PIDM,Loc+1,Acc++[C],Depth+1)
    end.

stdlib_strtol(PIDM,_Op,_Defines,_PC) ->
    Ptr = rvscorehw:load_register(PIDM,"a0"),
    Str = load_string(PIDM,Ptr),
    Val = case string:to_integer(Str) of
	{V,[]} ->
	    V;
	_R ->
	    throw({"rvslibs,strlib_strtol string not an integer", Str})
    end,
    rvscorehw:save_register(PIDM,"a0",Val),
    ok.

