%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_download).
-export([fetch/2, revision/2]).
%% internal exports
-export([git/1, git/2, process_port/2]).

revision(Spec, Directory) ->
	URL = proplists:get_value(url, Spec),
	Lines = revision_1(URL, Directory),
	case Lines of
		[] ->     {ok, ''};
		[Revision] -> {ok, string:strip(Revision, right, $\n)};
		Else -> {error, {unexpected, Else}}
	end.
			
revision_1(undefined, _) -> %% if no url is defined, don't do anything
    [];

revision_1({all, []}, _) ->
    [];

revision_1({all, [{Name, URL}|Rest]}, Directory) ->
    revision_1(URL, filename:join(Directory, Name)),
    revision_1({all, Rest}, Directory);

revision_1({git, __URL, __Ref}, Directory) ->
    case filelib:is_dir(Directory) of
        false -> {error, {enotdir, Directory}};
        true -> %% existing repo (or something else)
            %% Port = git(["rev-parse","--verify", "HEAD"], [{cd, Directory}, out, eof]),
			Port = git(["rev-parse","--verify", "HEAD"], [{cd, Directory}]),
            process_port(Port, fun(Revision) -> Revision end)
	end;
revision_1({hg, __URL, __Ref}, Directory) ->
    case filelib:is_dir(Directory) of
        false -> {error, {enotdir, Directory}};
        true -> %% existing repo (or something else)
            Port = hg(["identify","-i"], [{cd, Directory}, out, exit_status]),
            process_port(Port, fun(Revision) -> Revision end)
	end;
revision_1({svn, __URL, __Ref}, Directory) ->
    case filelib:is_dir(Directory) of
        false -> {error, {enotdir, Directory}};
        true -> %% existing repo (or something else)
            Port = svn(["svnversion","."], [{cd, Directory}, use_stdio, out, exit_status]),
            process_port(Port, fun(Revision) -> Revision end)
	end;
revision_1({bzr, __URL, __Ref}, Directory) ->
    case filelib:is_dir(Directory) of
        false -> {error, {enotdir, Directory}};
        true -> %% existing repo (or something else)
            Port = bzr(["revno"], [{cd, Directory}, use_stdio, out, exit_status]),
            process_port(Port, fun(Revision) -> Revision end)
        end.

fetch(Spec0, Directory) ->
    Fetch = fetch_1(proplists:get_value(url, Spec0), Directory),
	Spec = case revision(Spec0, Directory) of
			   {ok, CurrentRevision} -> 
				   [{revision, CurrentRevision}|Spec0];
			   {error, __Reason} -> 
				   Spec0
		   end,
    case file:open(filename:join(Directory,".agner.config"),[write]) of
        {ok, F} ->
            lists:foreach(fun (Term) ->
                                  io:fwrite(F,"~p.~n",[Term])
                          end, Spec),
            file:close(F);
        _ ->
            ignore
    end,
    Fetch.

fetch_1(undefined, _) -> %% if no url is defined, don't download anything
    ok;

fetch_1({all, []}, _) ->
    ok;

fetch_1({all, [{Name, URL}|Rest]}, Directory) ->
    fetch_1(URL, filename:join(Directory, Name)),
    fetch_1({all, Rest}, Directory);

fetch_1({git, URL, Ref}, Directory) ->
    case filelib:is_dir(Directory) of
        false -> %% clone
            PortClone = git(["clone", URL, Directory]),
            process_port(PortClone, fun (_) -> git_checkout(Ref, Directory) end);
        true -> %% existing repo (or something else)
            PortFetch = git(["fetch","origin"], [{cd, Directory}]),
            process_port(PortFetch, fun(_) -> git_checkout(Ref, Directory) end)
    end;

fetch_1({hg, URL, Rev}, Directory) ->
    case filelib:is_dir(Directory) of
        false -> %% new
            PortClone = hg(["clone", "-U", URL, Directory]),
            process_port(PortClone, fun (_) -> 
                                            PortUpdate = hg(["update", Rev], [{cd, Directory}]),
                                            process_port(PortUpdate, fun (_) -> ok end)
                                    end);
        true -> %% existing
            PortClone = hg(["pull", "-u", "-r", Rev],[{cd, Directory}]),
            process_port(PortClone, fun (_) -> ok end)
    end;

fetch_1({svn, Url, Rev}, Directory) ->
    io:format("[Fetching svn repository...]~n"),
    case filelib:is_dir(Directory) of
        false -> %% new
            PortCheckout = svn(["checkout", "-r", Rev, Url, filename:basename(Directory)],
                               [{cd, filename:dirname(Directory)}]),
            process_port(PortCheckout, fun (_) -> ok  end);
        true -> %% existing
            PortUp = svn(["up", "-r", Rev],[{cd, Directory}]),
            process_port(PortUp, fun (_) -> ok end)
    end.

%%

git_checkout({branch, Ref}, Directory) when is_list(Ref)->
    PortCheckout = git(["checkout","-q","origin/" ++ Ref],[{cd, Directory}]),
    process_port(PortCheckout, fun (_) -> ok end);
git_checkout({tag, Ref}, Directory) when is_list(Ref) ->
    PortCheckout = git(["checkout","-q",Ref],[{cd, Directory}]),
    process_port(PortCheckout, fun (_) -> ok end);
git_checkout(Ref, Directory) when is_list(Ref) ->
    PortCheckout = git(["checkout","-q",Ref],[{cd, Directory}]),
    process_port(PortCheckout, fun (_) -> ok end).
    
git(Args) ->
    git(Args,[]).

git(Args, Opts) ->
    Git = os:find_executable("git"),
    open_port({spawn_executable, Git},[{args, Args},
                                       exit_status|Opts]).

hg(Args) ->
    hg(Args,[]).

hg(Args, Opts) ->
    Hg = os:find_executable("hg"),
    open_port({spawn_executable, Hg},[{args, Args},
                                      exit_status|Opts]).

svn(Args, Opts) ->
    Svn = os:find_executable("svn"),
    open_port({spawn_executable, Svn},[{args, Args},
                                       exit_status|Opts]).

bzr(Args) ->
    bzr(Args,[]).

bzr(Args, Opts) ->
    Bzr = os:find_executable("bzr"),
    open_port({spawn_executable, Bzr},[{args, Args},
                                       exit_status|Opts]).

process_port(Port, Fun) ->
	process_port(Port, Fun, []).

process_port(Port, Fun, Acc) ->
    receive 
        {Port, {exit_status, 0}} ->
            apply(Fun, [lists:reverse(Acc)]);
		{Port, {data, D}} ->
			process_port(Port, Fun, [D|Acc]);
		{Port, eof} ->
			erlang:port_close(Port),
            apply(Fun, [lists:reverse(Acc)]);			
        {Port, {exit_status, Status}} ->
            {error, Status};
        {'EXIT', Port, PosixCode} ->
            {error, PosixCode}
    end.
