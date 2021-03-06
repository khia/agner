%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_server).
-ifdef(TEST).
-compile(export_all).
-endif. 
-include_lib("agner.hrl").
-include_lib("typespecs/include/typespecs.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([spec/2, spec_url/2, index/0, fetch/3, versions/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 60000).

-type dep_revision() :: {Version :: term(), Directory :: string(), Revision :: string()}.

-record(state, {
		  deps = [] :: [{Name :: string(), dep_revision()}],
		  requests = [] :: [
							{NameOrSpec :: term(), 
							 Version :: term(), 
							 Directory :: string()}],
		  active_requests = [] :: [{Name :: string()}]
         }).

-type gen_server_state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%%     Stops server.
%% @end
-spec(stop/1 :: (Pid :: pid()) -> ok).
stop(Pid) ->
    (catch gen_server:call(Pid, stop)),
    ok.

%% @doc Ask the server for a spec on Name and Version
%% @end
-spec spec(agner_package_name(), agner_package_version()) -> agner_spec().
                  
spec(Name, Version) ->
    gen_server:call(?SERVER, {spec, Name, Version}, ?TIMEOUT).

%% @doc Ask the server for a spec URL
%% @end
-spec spec_url(agner_package_name(), agner_package_version()) -> url().

spec_url(Name, Version) ->
    gen_server:call(?SERVER, {spec_url, Name, Version}, ?TIMEOUT).

%% @doc Ask the server for an index
%% @end
-spec index() -> list(agner_package_name()).
                   
index() ->
    gen_server:call(?SERVER, index, infinity).

%% @doc Fetch a package/project to a directory
%% @end
-spec fetch(agner_package_name(), agner_package_version(), directory()) -> ok | not_found_error();
           (agner_spec(), any(), directory()) -> ok | not_found_error().
                   
fetch(NameOrSpec, Version, Directory) ->
    Name = case io_lib:printable_list(NameOrSpec) of
			   true -> %% name
				   NameOrSpec;
			   false -> %% spec
				   proplists:get_value(name, NameOrSpec)
		   end,
	case gen_server:call(?SERVER, {fetch, Name, NameOrSpec, Version, Directory}, infinity) of
		{ok, {Directory, Revision}} -> 
			gen_server:call(?SERVER, {fetched, Name, {Version, Directory, Revision}}),
			{ok, Directory};
		Else ->
			gen_server:call(?SERVER, {unlock, Name}),
			Else
	end.

%% @doc Ask for the versions of a given package, Name
%% @end
-spec versions(agner_package_name()) -> list(agner_package_version()).
                      
versions(Name) ->
    gen_server:call(?SERVER, {versions, Name},?TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> gen_server_init_result().

init([]) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-type agner_call_spec() :: {spec, agner_package_name(), agner_package_version()}.
-type agner_call_spec_url() :: {spec_url, agner_package_name(), agner_package_version()}.
-type agner_call_index() :: index.
-type agner_call_fetch() :: {fetch, agner_package_name() | agner_spec(), agner_package_version(), directory()}.
-type agner_call_versions() :: {versions, agner_package_name()}.

-spec handle_call(agner_call_spec(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(agner_spec()|{error, bad_version}) ;
                 (agner_call_spec_url(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(url()|{error, bad_version}) ;
                 (agner_call_index(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(list(agner_package_name())) ;
                 (agner_call_fetch(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(ok | {error, any()}) ;
                 (agner_call_versions(), gen_server_from(), gen_server_state()) -> gen_server_async_reply(list(agner_package_version()) | not_found_error()).

handle_call(stop, _From, State)->
    {stop, normal, State};

handle_call({spec, Name, Version}, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_spec(Name, Version, From, indices())
			   end),
	{noreply, State};

handle_call({spec_url, Name, Version}, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_spec_url(Name, Version, From, indices())
			   end),
	{noreply, State};

handle_call(index, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_index(From, [], indices())
			   end),
	{noreply, State};

handle_call({fetch, Name, NameOrSpec, Version, Directory}, From, 
			#state{
					deps = Dependencies, 
					requests = Queue, active_requests = Locks} = State0) ->
	case lists:member(Name, Locks) of
		true ->
			State = State0#state{requests = [{NameOrSpec, Version, Directory}|Queue]};
		false ->
			State = State0#state{active_requests = [Name|Locks]},
			Revisions = proplists:append_values(Name, Dependencies),
			spawn_link(fun () ->
							   handle_fetch(NameOrSpec, Version, Directory, Revisions, From)
					   end)
	end,
	{noreply, State};

handle_call({fetched, Name, {Version, Directory, Revision}}, From, 
			#state{deps = Dependencies} = State0) ->
	State = State0#state{deps = [{Name, {Version, Directory, Revision}}|Dependencies]},
	handle_call({unlock, Name}, From, State);

handle_call({unlock, Name}, __From, 
			#state{requests = [], active_requests = Locks} = State0) ->
	State = State0#state{active_requests = lists:delete(Name, Locks)},
	{reply, ok, State};

handle_call({unlock, Name}, From, 
			#state{requests = [Query|Rest], active_requests = Locks} = State0) ->
	{NameOrSpec, Version, Directory} = Query,
	State = State0#state{requests = Rest, active_requests = lists:delete(Name, Locks)},
	handle_call({fetch, Name, NameOrSpec, Version, Directory}, From, State),
	{reply, ok, State};

handle_call({versions, Name}, From, #state{}=State) ->
	spawn_link(fun () ->
					   handle_versions(Name, From, indices())
			   end),
	{noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

-spec handle_info(any(), gen_server_state()) ->
						 gen_server_handle_info_result().

handle_info(_, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================                 
-spec handle_spec(agner_package_name(), agner_package_version(), gen_server_from(), agner_indices()) -> any().
handle_spec(_,_,From,[]) ->
	gen_server:reply(From, {error, not_found});
handle_spec(Name, Version, From, [{Mod0, Params}|Rest]) ->
	Mod = index_module(Mod0),
    case Mod:exists(Params, Name) of
        true ->
            case Mod:spec(Params, Name, Version) of
                {error, not_found} ->
                    handle_spec(Name, Version, From, Rest);
                Data ->
                    gen_server:reply(From, Data)
            end;
        false ->
            handle_spec(Name, Version, From, Rest)
    end.

-spec handle_spec_url(agner_package_name(), agner_package_version(), gen_server_from(), agner_indices()) -> any().
handle_spec_url(_,_,From,[]) ->
	gen_server:reply(From, {error, not_found});
handle_spec_url(Name, Version, From, [{Mod0, Params}|Rest]) ->
	Mod = index_module(Mod0),
    case sha1(Mod, Params, Name, Version) of
        SHA1 when is_list(SHA1) ->
            case Mod:exists(Params, Name) of
                true ->
                    case Mod:spec_url(Params, Name, SHA1) of
                        {error, not_found} ->
                            handle_spec_url(Name, Version, From, Rest);
                        URL ->
                            gen_server:reply(From, URL)
                    end;
                false ->
                    handle_spec_url(Name, Version, From, Rest)
            end;
        _ ->
            gen_server:reply(From, {error, bad_version})
    end.

-spec handle_index(gen_server_from(), list(agner_package_name()), list(tuple())) -> any().
handle_index(From, Acc, []) ->
    Repos = lists:reverse(Acc),
    RepoNames = lists:map(fun ({Name, _}) -> Name end, Repos),
    lists:foreach(fun (Name) ->
                          {ok, Pid} = agner_repo_server:create(Name, {flavour, "master"}),
                          agner_repo_server:set_pushed_at(Pid, binary_to_list(proplists:get_value(Name, Repos)))
                  end, RepoNames),
	gen_server:reply(From, lists:usort(RepoNames));
handle_index(From, Acc, [{Mod0, Params}|Rest]) ->
	Mod = index_module(Mod0),
	case Mod:repositories(Params) of
		{error, not_found} ->
			handle_index(From, Acc, Rest);
		Repos ->
            handle_index(From, lists:map(fun (Repo) -> indexize(Mod0, Params, Repo) end, Repos) ++ Acc, Rest)
	end.

-spec handle_fetch(agner_package_name() | agner_spec(), agner_package_version(), directory(), [dep_revision()], gen_server_from()) -> any().
handle_fetch(NameOrSpec, Version, Directory, Revisions, From) ->
    case io_lib:printable_list(NameOrSpec) of
        true ->
            case agner:spec(NameOrSpec, Version) of
                {error, _} = Error ->
                    gen_server:reply(From, Error);
                Spec ->
					Reply = do_fetch(Spec, Directory, Revisions),
                    gen_server:reply(From, Reply)
            end;
        false -> %% it is a spec
			Reply = do_fetch(NameOrSpec, Directory, Revisions),
			gen_server:reply(From, Reply)
    end.

-spec handle_versions(agner_package_name(), gen_server_from(), agner_indices()) -> any().
handle_versions(_,From,[]) ->
	gen_server:reply(From, {error, not_found});
handle_versions(Name, From, [{Mod0, Params}|Rest]) ->
	Mod = index_module(Mod0),
    case Mod:exists(Params, Name) of
        true ->
            case Mod:repository(Params, Name) of
                {error, not_found} ->
                    handle_versions(Name, From, Rest);
                _ ->
                    Branches = lists:map(fun
                                             ({[$%|_],_}) -> undefined;
                                             ({"gh-pages",_}) -> undefined;
                                             ({Branch, _}) -> {flavour, Branch} end,
                                         Mod:branches(Params, Name)),
                    Tags = lists:map(fun ({[$%|_],_}) -> undefined;
                                         ({Tag, _}) -> {release, Tag} end,
                                     Mod:tags(Params, Name)),
                    gen_server:reply(From, lists:filter(fun (undefined) -> false; (_) -> true end, Branches ++ Tags))
            end;
        false ->
            handle_versions(Name, From, Rest)
    end.

-spec sha1(agner_index(), agner_account(), agner_package_name(), agner_package_version()) -> sha1().
                  
sha1(Mod, Params, Name, Version) ->
    case Version of
        {flavour, Branch} ->
            Branch;
        {release, Tag} ->
            Tags = Mod:tags(Params, Name),
            proplists:get_value(Tag, Tags);
        no_such_version ->
            no_such_version
    end.

-spec(do_fetch/3 :: (agner_spec(), directory(), [dep_revision()]) -> {ok, string()} | {error, term()}). 
do_fetch(Spec, Directory, []) ->
	agner_download:fetch(Spec, Directory),
	case agner_download:revision(Spec, Directory) of
		{ok, Revision} ->		
			{ok, {Directory, Revision}};
		Else -> Else
	end;

do_fetch(Spec, Directory, Revisions) ->	
	TempDirectory = test_server:temp_name(Directory),	
	agner_download:fetch(Spec, TempDirectory),	
	case agner_download:revision(Spec, TempDirectory) of
		{ok, Revision} ->
			case choose_directory(Revision, Revisions, Directory, Spec) of
				{new, NewDirectory} -> % already fetched but different version
					file:rename(TempDirectory, NewDirectory),
					{ok, NewDirectory};
				{old, Directory} ->        % fetched for first time
					file:rename(TempDirectory, Directory),
					{ok, Directory};
				{old, NewDirectory} -> % already fetched 
					rm_dir(Directory),
					{ok, NewDirectory}
			end;
		Else -> Else
	end.

choose_directory(Revision, Revisions, Directory, Spec) ->
	case agner_utils:search_keys(Revision, 3, Revisions) of
		[] -> 
			case filelib:is_dir(Directory) of
				true -> 
					Version = proplists:get_value(version, Spec),
					{new, Directory ++ "-" ++ agner_spec:version_to_list(Version)};
				false ->								
					{old, Directory}
			end;
		[{__Version, NewDirectory, Revision}] -> 
			{old, NewDirectory}
	end.

rm_dir([$/|_] = Directory) ->
	%% Can be dangerous
	agner_utils:exec("rm -rf " ++ Directory, [{quiet,true}]);
rm_dir(Directory) ->	
	{error, {not_absolute_path, Directory}}.

index_module(T) ->
    case application:get_env(index_modules) of
        {ok, Modules} ->
            proplists:get_value(T, Modules);
        _ ->
            T
    end.

indexize(github, "agner", Name) ->
    Name;
indexize(github, Account, Name) ->
    Account ++ "/" ++ Name.

indices() ->
    case application:get_env(indices) of
        {ok, Val} ->
            Val;
        undefined ->
            []
    end.
