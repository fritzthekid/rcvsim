-module(rvsutils).
-compile(export_all).

%% From: (opposite to file:consult) https://zxq9.com/archives/1021
write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = unicode:characters_to_binary(lists:map(Format, List)),
    file:write_file(Filename, Text).

-ifdef(REBARTEST).
-include_lib("eunit/include/eunit.hrl").
write_terms_test() ->
    {ok, [Config]} = file:consult("data/config.config"),
    write_terms("test/outputs/config.config",[Config]),
    {ok, [MyConfig]} = file:consult("test/outputs/config.config"),
    ?assert(Config==MyConfig),
    ok.
-endif.
