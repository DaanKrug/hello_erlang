-module(simplemail_dataextractor).

-export([
		 get_subject/2,
		 get_content/2, 
		 get_tos_address/2,
		 get_status/2
		]).

get_subject(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"subject">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize(Value),
	case (string:length(Value2) > 200) of
		true ->
			string:slice(Value2, 0, 200);
		_ ->
			Value2
	end.
  
get_content(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"content">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(Value).

get_tos_address(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"tosAddress">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(Value).

get_status(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"status">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    Value3 = sanitizer_util:sanitize_all(Value2, false, 20, <<"a-z">>),
	case lists:member(Value3, [<<"awaiting">>, <<"reSend">>, <<"startProcessing">>,
							   <<"processing">>, <<"finished">>]) of
		true ->
			Value3;
		_ ->
			""
	end.
	