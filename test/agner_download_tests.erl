-module(agner_download_tests).
-include_lib("eunit/include/eunit.hrl").
-define(TARGET, agner_download).
revision_test() ->
    Spec = [{url, {git, undefined, undefined}}],
    {Result, Revision} = ?TARGET:revision(Spec, "."),
    ?assert(ok =:= Result),
    ?assert(is_list(Revision)),
    ?assert(erlang:length(Revision) > 0).
