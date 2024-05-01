-module(file_dataextractor).

-export([
		 get_name/2, 
		 get_link/2, 
		 get_file_id/2,
		 get_file_link/2,
		 get_file_name/2
		]).

get_file_id(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"fileId">>, BodyPropList, 0),
	Value2 = sanitizer_util:sanitize_all(Value,true,0,undefined),
	% Value3 = list_to_integer(string_util:to_string(Value2)),
	Value3 = number_util:to_integer(Value2),
   	number_util:coalesce(Value3, DefaultValue, true).

get_file_link(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"fileLink">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), false, 0, <<"url">>),
    case (validator:url(Value2)) of
		ok -> 
			Value2;
		_ ->
			""
	end.

get_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"name">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 30, <<"A-z0-9Name">>).
 
get_link(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"link">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), false, 0, <<"url">>),
    case (validator:url(Value2)) of
		ok -> 
			Value2;
		_ ->
			""
	end.

get_file_name(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"filename">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"A-z0-9">>),
	Extension = file_util:get_extension(Value2),
	case (lists:member(Extension, file_util:valid_upload_extensions())) of
		true ->
			lists:concat([
						  erlang:system_time(millisecond),
                          string_util:generate_random_filename(190), 
                          ".", 
                          Extension
                         ]);
		_ ->
			undefined
	end.

  