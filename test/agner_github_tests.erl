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
    %% This test works only once
    application:start(inets),
    inets:start(),
    ssl:start(),
    inets:start(httpc,[{profile, agner}]),
    Name = "test.agner",
    [AgnerName|_] = string:tokens(Name, "."), %% "test.agner" -> "test"
    User = os:getenv("USERNAME"),
    {__Result, Account} = ?TARGET:get_git_account(User),
    Res = case ?TARGET:exists(Account, AgnerName) of
	      true -> 
		  Msg = io_lib:format("IGNORING: Repository ~p already exist on github~n", [Name]),
		  ?debugMsg(Msg),
		  ok;
	      __Else ->
		  ?TARGET:new_repository(Name, User)
	  end,
    inets:stop(),
    ssl:stop(),
    ?assert(Res =:= ok).
