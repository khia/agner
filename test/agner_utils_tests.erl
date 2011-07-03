-module(agner_utils_tests).
-include_lib("eunit/include/eunit.hrl").
-define(TARGET, agner_utils).
exec_test() ->
    FileName = test_server:temp_name("/tmp/"),
    ?TARGET:exec("touch " ++ FileName),
    ?assertCmd("rm " ++ FileName).

