-module(wallettransaction_dataextractor).

-export([
		 get_identifier_sender/2,
		 get_identifier_receiver/2,
		 get_description/2,
		 get_amount_coins/2,
		 get_status/2
		]).

get_identifier_sender(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"identifierSender">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"A-z0-9">>).

get_identifier_receiver(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"identifierReceiver">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"A-z0-9">>).

get_description(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"description">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"A-z0-9">>).

get_amount_coins(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"amountCoins">>, BodyPropList, DefaultValue),
	Value2 = sanitizer_util:sanitize_all(Value, true, 28, <<"0-9">>),
	Value3 = number_util:to_float(Value2),
	case (Value3 < 0) of
		true ->
			0;
		_ ->
			Value3
	end.

get_status(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"status">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    Value3 = sanitizer_util:sanitize_all(Value2, false, 30, <<"a-z">>),
	ValidStatus = [<<"commited">>, <<"rolledback">>],
    case (lists:member(Value3, ValidStatus)) of
		true -> 
			Value3;
		_ ->
			""
	end.
	