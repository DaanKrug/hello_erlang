-module(json_util).

-export([encode/1, encode_map/1, decode/1]).

encode(PropsList) ->
	case (datatransform_util:is_emptylist(PropsList)) of
		true ->
			"[]";
		_ ->
			jiffy:encode(PropsList)
	end.

encode_map(Map) ->
	case (datatransform_util:is_undefined(Map)) of
		true ->
			"{}";
		_ ->
			jiffy:encode(Map)
	end.

decode(JsonString) ->
    case (datatransform_util:is_emptystring(JsonString)) of
		true ->
			jiffy:decode("{}", [copy_strings]);
		_ ->
			jiffy:decode(JsonString, [copy_strings])
	end.
	
