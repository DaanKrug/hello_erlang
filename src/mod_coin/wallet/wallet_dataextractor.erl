-module(wallet_dataextractor).

-export([
		 get_identifier/2, 
		 get_recoverinfo/2, 
		 get_password/1,
		 get_new_password/1
		]).

get_identifier(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"identifier">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"A-z0-9">>).
 
get_recoverinfo(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"recoverinfo">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    sanitizer_util:sanitize(Value2).
	
get_password(BodyPropList) ->
    Value = proplists:get_value(<<"password">>, BodyPropList, ""),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"A-z0-9">>).

get_new_password(BodyPropList) ->
    Value = proplists:get_value(<<"new_password">>, BodyPropList, ""),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"A-z0-9">>).
