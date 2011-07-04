-module(agner_github_tests).
-include_lib("eunit/include/eunit.hrl").
-define(TARGET, agner_github).
get_git_account_test() ->
    User = os:getenv("USERNAME"),
    {Result, Account0} = ?TARGET:get_git_account(User),
    ?assert(ok =:= Result),
    Account = Account0 ++ "\n",
    ?assertCmdOutput(Account, "git config --global --get github.user").

get_git_token_test() ->
    User = os:getenv("USERNAME"),
    {Result, Token0} = ?TARGET:get_git_token(User),
    ?assert(ok =:= Result),
    Token = Token0 ++ "\n",
    ?assertCmdOutput(Token, "git config --global --get github.token").

new_repository_test() ->
    inets:start(),
    ssl:start(),
    Name = "test.agner",
    User = os:getenv("USERNAME"),
    Res = ?TARGET:new_repo(Name, User),
    ssl:stop(),
    inets:start(),
    ?assert(Res =:= ok).
