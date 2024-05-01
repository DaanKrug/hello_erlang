-module(appconfig_dataextractor).

-export([
		 get_name/2, 
		 get_description/2, 
		 get_site/2, 
		 get_pricing_policy/2,
		 get_privacity_policy/2, 
		 get_useterms_policy/2, 
		 get_usecontract_policy/2,
		 get_author_info/2
		]).

get_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"name">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 50, <<"A-z0-9Name">>).
  
get_description(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"description">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"A-z0-9">>).
  
get_site(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"site">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"url">>).
  
get_pricing_policy(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"pricingPolicy">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).

get_privacity_policy(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"privacityPolicy">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).  

get_useterms_policy(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"usetermsPolicy">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).  
  
get_usecontract_policy(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"usecontractPolicy">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).  
  
get_author_info(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"authorInfo">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).  
