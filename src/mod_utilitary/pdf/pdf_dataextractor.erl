-module(pdf_dataextractor).

-export([get_html/1, get_orientation/1]).

get_html(BodyPropList) ->
	Value = proplists:get_value(<<"html">>, BodyPropList, ""),
    sanitizer_util:sanitize(string_util:trim(Value)).
	
get_orientation(BodyPropList) ->
    Value = proplists:get_value(<<"orientation">>, BodyPropList, ""),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), false, 9, <<"A-z">>),
	case lists:member(Value2, [<<"portrait">>, <<"landscape">>]) of
		true ->
			Value2;
		_ ->
			"portrait"
	end.
	