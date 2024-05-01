-module(module_dataextractor).

-export([get_name/2]).

get_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"name">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    Value3 = sanitizer_util:sanitize_all(Value2, false, 20, <<"A-z0-9Name">>),
	ValidModules = [
					<<"file">>, <<"menu">>, 
                    <<"register">>, <<"s3upload">>, <<"itaubankconfig">>
				   ],
	case (lists:member(Value3, ValidModules)) of
		true -> 
			Value3;
		_ ->
			""
	end.
