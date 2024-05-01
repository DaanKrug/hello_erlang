-module(mailerconfig_dataextractor).

-export([
		 get_provider/2, 
		 get_name/2,
		 get_user_name/2,
		 get_password/2,
		 get_per_month/2,
		 get_per_day/2,
		 get_per_hour/2,
		 get_per_minute/2,
		 get_per_second/2,
		 get_replay_to/2
		]).

get_provider(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"provider">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    Value3 = sanitizer_util:sanitize_all(Value2, false, 30, <<"A-z0-9">>),
	ValidProviders = [
					  <<"skallerten">>, <<"gmail">>, <<"mailgun">>, <<"mailjet">>,
					  <<"sendinblue">>, <<"sparkpost">>, <<"sendgrid">>, <<"smtp2go">>,
					  <<"elasticemail">>, <<"iagente">>, <<"socketlab">>, <<"postmark">>
					 ],
    case (lists:member(Value3, ValidProviders)) of
		true -> 
			Value3;
		_ ->
			""
	end.

get_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"name">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 30, <<"A-z0-9">>).

get_user_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"username">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"email">>).

get_password(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"password">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"password">>).

get_per_month(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"perMonth">>, BodyPropList, DefaultValue),
	Value2 = sanitizer_util:sanitize_all(Value, true, 10, undefined),
	% Value3 = list_to_integer(string_util:to_string(Value2)),
	Value3 = number_util:to_integer(Value2),
    number_util:coalesce_interval(Value3, 0, number_util:max_integer()).

get_per_day(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"perDay">>, BodyPropList, DefaultValue),
	Value2 = sanitizer_util:sanitize_all(Value, true, 10, undefined),
	% Value3 = list_to_integer(string_util:to_string(Value2)),
	Value3 = number_util:to_integer(Value2),
    number_util:coalesce_interval(Value3, 0, number_util:max_integer()).

get_per_hour(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"perHour">>, BodyPropList, DefaultValue),
	Value2 = sanitizer_util:sanitize_all(Value, true, 10, undefined),
	% Value3 = list_to_integer(string_util:to_string(Value2)),
	Value3 = number_util:to_integer(Value2),
    number_util:coalesce_interval(Value3, 1, number_util:max_integer()).

get_per_minute(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"perMinute">>, BodyPropList, DefaultValue),
	Value2 = sanitizer_util:sanitize_all(Value, true, 10, undefined),
	% Value3 = list_to_integer(string_util:to_string(Value2)),
	Value3 = number_util:to_integer(Value2),
    number_util:coalesce_interval(Value3, 1, number_util:max_integer()).

get_per_second(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"perSecond">>, BodyPropList, DefaultValue),
	Value2 = sanitizer_util:sanitize_all(Value, true, 10, undefined),
	% Value3 = list_to_integer(string_util:to_string(Value2)),
	Value3 = number_util:to_integer(Value2),
    number_util:coalesce_interval(Value3, 1, number_util:max_integer()).

get_replay_to(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"replayTo">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 100, <<"email">>).

  