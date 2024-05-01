-module(pagemenuitem_dataextractor).

-export([get_content/2, get_page_menu_id/2]).

get_content(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"content">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize(string_util:trim(Value)).

get_page_menu_id(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"pageMenuId">>, BodyPropList, 0),
	Value2 = sanitizer_util:sanitize_all(Value, true, 0, undefined),
	% Value3 = list_to_integer(string_util:to_string(Value2)),
	Value3 = number_util:to_integer(Value2),
    number_util:coalesce(Value3, DefaultValue, true).