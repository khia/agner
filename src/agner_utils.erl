%% -*- Mode: Erlang; tab-width: 4 -*-
-module(agner_utils).
-export([launch_browser/1, exec/1, exec/2, search_keys/3]).

launch_browser(URL) ->
    case os:type() of
        {unix, darwin} ->
            os:cmd("open " ++ URL);
        {unix, linux} ->
            os:cmd("firefox " ++ URL ++ " &")
    end.

exec(Command) ->
	exec(Command, []).

exec(Command, Opts0) ->
	Quiet = proplists:get_value(quiet, Opts0, false),
	Opts = lists:keydelete(quiet, 1, Opts0),
	Port = open_port({spawn,"sh -c \"" ++ Command ++ "\""},
					 [exit_status,stderr_to_stdout,use_stdio, stream|Opts]),
	unlink(Port),
	PortHandler = fun (F) ->
						  receive
							  {'EXIT', Port, normal} ->
								  {ok, []};
							  {'EXIT', Port, Reason} ->
								  {error, Reason};
							  {Port,{exit_status,0}} ->
								  {ok, []};
							  {Port,{exit_status,_} = Reason} ->
								  {error, Reason};
							  {Port, {data, D}} when not Quiet andalso is_list(D) ->
								  io:format("~s",[D]),
								  F(F);
							  _ ->
								  F(F)
						  end
				  end,
	Result = PortHandler(PortHandler),
	receive
		{'EXIT', Port, normal} -> %% flush port exit
			ok
	after 0 ->
			ok
	end,
	Result.
	
search_keys(Key, Idx, List) ->
    search_keys(Key, Idx, List, []).
search_keys(__Key, __Idx, [], Res) -> Res;
search_keys(Key, Idx, [Element | Rest], Res0) ->
    Res1 = case (element(Idx, Element) =:= Key) of
	    true -> [Element] ++ Res0;
	    false -> Res0
	end,    
    search_keys(Key, Idx, Rest, Res1).	
