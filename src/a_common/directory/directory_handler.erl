-module(directory_handler).

-behaviour(cowboy_rest).

-export(
   		[
		 init/2, 
		 allowed_methods/2, 
		 resource_exists/2, 
		 content_types_provided/2, 
		 content_types_accepted/2,
		 get_file/2
		]
	   ).

init(Req, Paths) ->
	{cowboy_rest, Req, Paths}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

resource_exists(Req, {ReqPath, FilePath}) ->
	case file:list_dir(FilePath) of
		{ok, Fs} -> {true, Req, {ReqPath, lists:sort(Fs)}};
		_Err -> {false, Req, {ReqPath, FilePath}}
	end.

content_types_provided(Req, State) ->
	{[{{<<"text">>, <<"html">>, []}, get_file}], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"text">>, <<"html">>, '*'}, get_file}], Req, State}.

get_file(Req, {Path, Fs}) ->
	Files = [ <<(list_to_binary(F))/binary>> || F <- Fs ],
	{lists:nth(1, Files), Req, Path}.









