-module(agner_server_tests).
-include_lib("eunit/include/eunit.hrl").
-define(TARGET, agner_server).
fetch_by_spec_test() ->
    Server = ?TARGET:start_link(),
    Spec = [{url,{git,"https://github.com/mochi/erl_img.git",{branch,"master"}}}],
    DirName = test_server:temp_name("/tmp/"),
    io:format("DirName: ~p~n", [DirName]),
    {Result, NewDirectory} = ?TARGET:fetch(Spec, "@master", DirName),
    ?TARGET:stop(Server),
    ?TARGET:rm_dir(DirName),
    io:format("Result: ~p~n", [{Result, NewDirectory}]),
    ?assert(ok =:= Result),
    ?assert(is_list(NewDirectory)),
    ?assert(erlang:length(NewDirectory) > 0).
