-module(itaubankconfig_dataextractor).

-export([
		 get_a1_itaukey/2,
		 get_a2_shoplinecode/2,
		 get_a3_shoplinekey/2,
		 get_a4_shoplinesite/2,
		 get_order_number/2,
		 get_order_value/2,
		 get_observation/2,
		 get_owner_name/2,
		 get_cpf_inscription/2,
		 get_cpf_number/2,
		 get_owner_address/2,
		 get_owner_neighborhood/2,
		 get_owner_cep/2,
		 get_owner_city/2,
		 get_owner_state/2,
		 get_expires_date/2,
		 get_url_retorno/2,
		 get_additional_text1/2,
		 get_additional_text2/2,
		 get_additional_text3/2,
		 get_format/2
		]).

get_a1_itaukey(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"a1_itaukey">>, BodyPropList, DefaultValue),
	Value2 = string:uppercase(string_util:trim(Value)),
    sanitizer_util:sanitize_all(Value2, false, 50, <<"A-z0-9">>).

get_a2_shoplinecode(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"a2_shoplinecode">>, BodyPropList, DefaultValue),
	Value2 = string:uppercase(string_util:trim(Value)),
    sanitizer_util:sanitize_all(Value2, false, 50, <<"A-z0-9">>).

get_a3_shoplinekey(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"a3_shoplinekey">>, BodyPropList, DefaultValue),
	Value2 = string:uppercase(string_util:trim(Value)),
    sanitizer_util:sanitize_all(Value2, false, 50, <<"A-z0-9">>).

get_a4_shoplinesite(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"a4_shoplinesite">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    sanitizer_util:sanitize_all(Value2, false, 250, <<"A-z0-9">>).

get_order_number(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"orderNumber">>, BodyPropList, DefaultValue),
	Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), true, 8, <<"0-9">>),
	string_util:left_zeros(Value2, 8).

get_order_value(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"orderValue">>, BodyPropList, DefaultValue),
    Value2 = number_util:to_float_format(string_util:trim(Value), 2, true),
	sanitizer_util:sanitize_all(Value2, true, 11, <<"money">>).

get_observation(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"observation">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).

get_owner_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"ownerName">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"A-z">>).
  
get_cpf_inscription(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"cpfInscription">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), true, 2, <<"0,1,2">>),
	string_util:right_spaces(Value2, 2).

get_cpf_number(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"cpfNumber">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), true, 14, <<"0-9">>).

get_owner_address(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"ownerAddress">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"A-z0-9">>).

get_owner_neighborhood(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"ownerNeighborhood">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"A-z0-9">>).

get_owner_cep(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"ownerCep">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), true, 8, <<"0-9OnlyNums">>),
	case lists:member(string:length(Value2), [0,8]) of
		true ->
			Value2;
		_ ->
			""
	end.

get_owner_city(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"ownerCity">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"A-z0-9">>).

get_owner_state(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"ownerState">>, BodyPropList, DefaultValue),
	Value2 = string:uppercase(string_util:trim(Value)),
    sanitizer_util:sanitize_all(Value2, false, 2, <<"A-z">>).
  
get_expires_date(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"expiresDate">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    Value3 = sanitizer_util:sanitize_all(Value2, true, 8, <<"0-9OnlyNums">>),
	case lists:member(string:length(Value3), [0,8]) of
		true ->
			Value3;
		_ ->
			""
	end.

get_url_retorno(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"urlRetorno">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).

get_additional_text1(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"additionalText1">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).

get_additional_text2(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"additionalText2">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).

get_additional_text3(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"additionalText3">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).

get_format(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"format">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), true, 1, <<"0-9">>),
	case (Value2 == <<"1">>) of
		true ->
			"1";
		_ ->
			"0"
	end.

  