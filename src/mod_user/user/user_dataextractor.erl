-module(user_dataextractor).

-export([
		 get_user_id/2, 
		 get_user_name/2, 
		 get_user_email/2, 
		 get_confirmation_code/2,
		 get_name/2, 
		 get_email/2, 
		 get_password/1,
		 get_category/2,
		 get_permissions/2
		]).

get_user_id(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"userId">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), true, 20, undefined),
    % list_to_integer(string_util:to_string(Value2)),
	number_util:to_integer(Value2).

get_user_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"userName">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 30, <<"A-z">>).

get_user_email(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"userEmail">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    sanitizer_util:sanitize_all(Value2, false, 100, <<"email">>).
 
get_confirmation_code(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"confirmation_code">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 20, <<"A-z0-9">>).

get_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"name">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 30, <<"A-z">>).
 
get_email(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"email">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    sanitizer_util:sanitize_email(Value2).
	
get_password(BodyPropList) ->
    Value = proplists:get_value(<<"password">>, BodyPropList, ""),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"A-z0-9">>).

get_category(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"category">>, BodyPropList, DefaultValue),
    Value2 = string:lowercase(string_util:trim(Value)),
    Value3 = sanitizer_util:sanitize_all(string_util:trim(Value2), false, 15, <<"a-z">>),
    case lists:member(Value3, [<<"admin_master">>, <<"admin">>, 
                               <<"system_auditor">>, <<"enroll">>, <<"external">>]) of
		true ->
			Value3;
		_ ->
			""
	end.

get_permissions(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"permissions">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string:lowercase(string_util:trim(Value))).
