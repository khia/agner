-module(agner_download_tests).
-include_lib("eunit/include/eunit.hrl").
-define(TARGET, agner_download).
git_fresh_fetch_test() ->
    Spec = [{url,{git, data_dir(), {branch,"master"}}}],
    DirName = test_server:temp_name("/tmp/"),
    Result = ?TARGET:fetch(Spec, DirName),
    agner_server:rm_dir(DirName),
    ?assert(ok =:= Result).

git_update_fetch_test() ->
    Spec = [{url,{git, data_dir(), {branch,"master"}}}],
    DirName = test_server:temp_name("/tmp/"),
    Result0 = ?TARGET:fetch(Spec, DirName),
    Result1 = ?TARGET:fetch(Spec, DirName),
    agner_server:rm_dir(DirName),
    ?assert(ok =:= Result0),
    ?assert(ok =:= Result1).

git_revision_test() ->
    Spec = [{url, {git, undefined, undefined}}],
    {Result, Revision} = ?TARGET:revision(Spec, "."),
    ?assert(ok =:= Result),
    ?assert(is_list(Revision)),
    ?assert(erlang:length(Revision) > 0).

bzr_fresh_fetch_test() ->
    Spec = [{url,{bzr, data_dir(), {last, "1"}}}],
    DirName = test_server:temp_name("/tmp/"),
    Result = ?TARGET:fetch(Spec, DirName),
    agner_server:rm_dir(DirName),
    ?assert(ok =:= Result).

bzr_update_fetch_test() ->
    Spec = [{url,{bzr, data_dir(), {last, "1"}}}],
    DirName = test_server:temp_name("/tmp/"),
    Result0  = ?TARGET:fetch(Spec, DirName),
    Result1 = ?TARGET:fetch(Spec, DirName),
    agner_server:rm_dir(DirName),
    ?assert(ok =:= Result0),
    ?assert(ok =:= Result1).

bzr_revision_test() ->
    Spec = [{url, {bzr, undefined, undefined}}],
    {Result, Revision} = ?TARGET:revision(Spec, data_dir()),
    ?assert(ok =:= Result),
    ?assert(is_list(Revision)),
    ?assert(erlang:length(Revision) > 0).

hg_revision_test() ->
    Spec = [{url, {hg, undefined, undefined}}],
    {Result, Revision} = ?TARGET:revision(Spec, data_dir()),
    ?assert(ok =:= Result),
    ?assert(is_list(Revision)),
    ?assert(erlang:length(Revision) > 0).

data_dir() ->    
    %% eunit changes the directory so we cannot rely on file:get_cwd/0
    Top = os:getenv("PWD"),
    Top ++ "/test/data_dir/".
    
    
