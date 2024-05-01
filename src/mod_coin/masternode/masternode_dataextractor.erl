-module(masternode_dataextractor).

-export([
		 get_a1_name/2, 
		 get_a2_ip/2, 
		 get_a3_region/2,
		 get_a5_walletid/2,
		 get_a6_walletidentifier/2
		]).

get_a1_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"a1_name">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"A-z0-9">>).
 
get_a2_ip(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"a2_ip">>, BodyPropList, DefaultValue),
	sanitizer_util:sanitize_all(string_util:trim(Value), false, 30, <<"0-9">>).
	
get_a3_region(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"a3_region">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), false, 50, <<"A-z0-9">>),
	ValidRegions = [<<"north_america">>, <<"center_america">>, <<"south_america">>],
	case (lists:member(Value2, ValidRegions)) of
		true ->
			Value2;
		_ ->
			lists:nth(1, ValidRegions)
	end.

get_a5_walletid(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"a5_walletid">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), true, 20, <<"0-9">>).

get_a6_walletidentifier(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"a6_walletidentifier">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"A-z0-9">>).






