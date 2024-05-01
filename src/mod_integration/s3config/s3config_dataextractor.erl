-module(s3config_dataextractor).

-export([
		 get_bucket_name/2, 
		 get_bucket_url/2,
		 get_region/2,
		 get_version/2,
		 get_key/2,
		 get_secret/2
		]).

get_bucket_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"bucketName">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"A-z0-9">>).
  
get_bucket_url(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"bucketUrl">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"A-z0-9">>).

get_region(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"region">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 30, <<"A-z0-9">>).
  
get_version(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"version">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 30, <<"A-z0-9">>).
  
get_key(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"key">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 50, <<"A-z0-9">>).
  
get_secret(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"secret">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"A-z0-9">>).

  