-module(rest_service).

-export([parse_req_get_method/3]).

parse_req_get_method(Prefix, HttpMethod, Path) ->
	Methods = path_methods(),
	get_method(Prefix, HttpMethod, Path, Methods, 1, length(Methods)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_method(Prefix, HttpMethod, Path, Methods, Count, Length) ->
	case (Count > Length) of
		true -> 
			undefined;
		_ ->
			MethodToCallAndParams = validate_method(Prefix, HttpMethod, Path, lists:nth(Count, Methods)),
			case (undefined /= MethodToCallAndParams) of
				true ->
					MethodToCallAndParams;
				_ ->
					get_method(Prefix, HttpMethod, Path, Methods, Count + 1, Length)
			end
	end.

validate_method(Prefix, HttpMethod, Path, Method2) ->
	case (string_util:equals(HttpMethod, lists:nth(1, Method2))) of
		true -> 
			Prefix2 = datatransform_util:list_to_binary_format(Prefix),
			Path2 = datatransform_util:list_to_binary_format(Path),
			get_method_name_and_params(Prefix2, Method2, Path2);
		_ ->
			undefined
	end.
	
get_method_name_and_params(Prefix, Method, Path) ->
	Prefix2 = datatransform_util:list_to_binary_format(lists:nth(3, Method)),
	Path2 = string:find(Path, Prefix),
	case Path2 of
		nomatch ->
			undefined;
		_ ->
			Path3 = string:find(Path2, Prefix2),
			case Path3 of 
				nomatch ->
					undefined;
				_ ->
					Path4 = datatransform_util:list_to_binary_format(Path3),
					Path5 = string:find(Path4, <<"/">>),
					case Path5 of
						nomatch ->
							undefined;
						_ ->
							[_Header | Tail] = string:split(Path5, <<"/">>, all),
							[_Header2 | Params] = Tail,
							case length(Params) == lists:nth(2, Method) of
								true -> 
									[lists:nth(4, Method) | Params];
								_ ->
									undefined
							end
					end
			end
	end.
		
path_methods() ->
	[
	 [<<"PATCH">>, 1, "/unDrop",        restore],
	 [<<"PATCH">>, 1, "/trullyDrop",    trully_delete],
	 [<<"POST">>,  0, "/",              save],
	 [<<"POST">>,  1, "/",              edit],
	 [<<"POST">>,  2, "/",              load_all],
	 [<<"PUT">>,   1, "/",              update],
	 [<<"PATCH">>, 1, "/",              delete],
	 [<<"PUT">>,   0, "/",         	    special_op1],
	 [<<"PATCH">>, 0, "/",              special_op2],
	 [<<"POST">>,  0, "/fileS3Upload",  special_op1]
	].
