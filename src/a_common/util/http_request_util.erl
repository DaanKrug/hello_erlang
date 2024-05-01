-module(http_request_util).

-export(
   		[
		 make_get_request/2,
		 make_post_request/3,
		 make_put_request/3,
		 make_patch_request/3,
		 make_delete_request/3
		]
	   ).

make_get_request(Url, Debugg) ->
    make_request(get, {Url, []}, [], [], Debugg).
  
make_post_request(Url, JsonBody, Debugg) ->
    make_request(post, {Url, [], <<"application/json">>, JsonBody}, [], [], Debugg).
  
make_put_request(Url, JsonBody, Debugg) ->
    make_request(put, {Url, [], <<"application/json">>, JsonBody}, [], [], Debugg).

make_patch_request(Url, JsonBody, Debugg) ->
    make_request(patch, {Url, [], <<"application/json">>, JsonBody}, [], [], Debugg).

make_delete_request(Url, JsonBody, Debugg) ->
    make_request(delete, {Url, [], <<"application/json">>, JsonBody}, [], [], Debugg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_request(Method, Request, HTTPOptions, Options, Debugg) ->
	Result = httpc:request(Method, Request, HTTPOptions, Options, Debugg),
	handle_response(Result, Debugg).
  
handle_response(Result, Debugg) ->
	List = tuple_to_list(Result),
	Key = lists:nth(1, List),
	Value = lists:nth(2, List),
	case Key of
		ok ->
			case Value of
				saved_to_file ->
					debugg(ok, saved_to_file, Debugg);
				_ ->
					debugg(Value, Value, Debugg)
			end;
		error ->
			debugg(error, Value, Debugg)
	end.
	
debugg(Result, PrintValue, Debugg) ->
	case Debugg of 
		true ->
			io:format(PrintValue),
			Result;
		_ ->
			Result
	end.
	