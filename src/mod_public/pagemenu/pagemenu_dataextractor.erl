-module(pagemenu_dataextractor).

-export([get_name/2]).

get_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"name">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"url">>).
