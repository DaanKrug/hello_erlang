-module(apptheme_dataextractor).

-export([
		 get_name/2, 
		 get_image/2,
		 get_default_theme/2,
		 get_themes/2,
		 get_organization/2,
		 get_image_height/2,
		 get_position_x/2,
		 get_position_y/2,
		 get_background_width/2,
		 get_background_height/2,
		 get_background_repeat/2
		]).

get_name(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"name">>, BodyPropList, DefaultValue),
    sanitizer_util:sanitize_all(string_util:trim(Value), false, 30, <<"A-z0-9">>).

get_image(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"image">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), false, 50, <<"A-z0-9">>),
	ValidImages = [
				   <<"aircity-min.jpg">>, <<"aircity2-min.jpg">>, <<"autumforest-min.jpg">>, 
				   <<"bardcup-min.jpg">>, <<"calw01-min.jpg">>, <<"calw02-min.jpg">>, <<"calw03-min.jpg">>, 
				   <<"calw04-min.jpg">>, <<"calw05-min.jpg">>, <<"camping-min.jpg">>, <<"citylogic-min.jpg">>, 
				   <<"cups-min.jpg">>, <<"curvedsunset-min.jpg">>, <<"dresden01-min.jpg">>, <<"dresden02-min.jpeg">>, 
			       <<"dresden03-min.jpg">>, <<"dresden04-min.jpg">>, <<"dresden05-min.jpg">>, <<"dresden06-min.jpg">>, 
				   <<"dresden07-min.jpg">>, <<"dresden08-min.jpg">>, <<"dresden09-min.jpg">>, <<"flow-min.jpg">>, 
				   <<"flowermountain-min.jpg">>, <<"hemingway-min.jpg">>, <<"lanterns-min.jpg">>, <<"lowforest-min.jpg">>, 
				   <<"nightcity-min.jpg">>, <<"path-min.jpg">>, <<"pinkbridge-min.jpg">>, <<"rivercity-min.jpg">>, 
				   <<"road-min.jpg">>, <<"schneestrasse-min.jpg">>, <<"smogpark-min.jpg">>, <<"sunset-min.jpg">>, 
				   <<"sunshineonroad-min.jpg">>, <<"venicesunshine-min.jpg">>, <<"wintermountain-min.jpg">>
				  ],
	case lists:member(Value2, ValidImages) of
		true ->
			Value2;
		_ ->
			lists:nth(1, ValidImages)
	end.
 
get_default_theme(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"defaultTheme">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), false, 20, <<"A-z0-9">>),
	ValidThemes = [
				   <<"blue01">>, <<"blue02">>, <<"blue03">>, <<"blue04">>, <<"dark01">>, 
				   <<"dark02">>, <<"dark03">>, <<"dark04">>, <<"dark05">>, <<"green01">>, 
				   <<"green02">>, <<"green03">>, <<"orkut">>, <<"red01">>, <<"red02">>, <<"red03">>
				  ],
	case lists:member(Value2, ValidThemes) of
		true ->
			Value2;
		_ ->
			lists:nth(1, ValidThemes)
	end.

get_themes(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"themes">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), false, 250, <<"A-z0-9">>),
	IsEmpty = string:is_empty(string:replace(Value2, ",", "", all)),
	case IsEmpty of
		true ->
			case (datatransform_util:is_emptystring(DefaultValue)) of
				true ->
					get_default_theme(BodyPropList, "");
				_ ->
					DefaultValue
			end;
		_ ->
			Value2
	end.

get_organization(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"organization">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), false, 10, <<"A-z0-9">>),
	ValidOrganizations = [
						  <<"mode01">>, <<"mode02">>, <<"mode03">>, 
						  <<"mode04">>, <<"mode05">>, <<"mode06">>, <<"mode07">>
						 ],
	case lists:member(Value2, ValidOrganizations) of
		true ->
			Value2;
		_ ->
			lists:nth(1, ValidOrganizations)
	end.

get_image_height(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"imageHeight">>, BodyPropList, DefaultValue),
	Value2 = sanitizer_util:sanitize_all(Value, true, 2, <<"0-9">>),
	% Value3 = list_to_integer(string_util:to_string(Value2)),
	Value3 = number_util:to_integer(Value2),
    number_util:coalesce_interval(Value3, 4, 20).

get_position_x(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"positionX">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    Value3 = sanitizer_util:sanitize_all(Value2, false, 10, <<"a-z">>),
	ValidPositionsX = [<<"center">>, <<"left">>, <<"right">>],
	case lists:member(Value3, ValidPositionsX) of
		true ->
			Value3;
		_ ->
			lists:nth(1, ValidPositionsX)
	end.

get_position_y(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"positionY">>, BodyPropList, DefaultValue),
	Value2 = string:lowercase(string_util:trim(Value)),
    Value3 = sanitizer_util:sanitize_all(Value2, false, 10, <<"a-z">>),
	ValidPositionsY = [<<"center">>, <<"top">>, <<"bottom">>],
	case lists:member(Value3, ValidPositionsY) of
		true ->
			Value3;
		_ ->
			lists:nth(1, ValidPositionsY)
	end.

get_background_width(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"backgroundWidth">>, BodyPropList, DefaultValue),
	Value2 = sanitizer_util:sanitize_all(Value, true, 3, <<"0-9">>),
	% Value3 = list_to_integer(string_util:to_string(Value2)),
	Value3 = number_util:to_integer(Value2),
    number_util:coalesce_interval(Value3, 10, 100).

get_background_height(BodyPropList, DefaultValue) ->
	Value = proplists:get_value(<<"backgroundHeight">>, BodyPropList, DefaultValue),
	Value2 = sanitizer_util:sanitize_all(Value, true, 3, <<"0-9">>),
	% Value3 = list_to_integer(string_util:to_string(Value2)),
	Value3 = number_util:to_integer(Value2),
    number_util:coalesce_interval(Value3, 10, 100).

get_background_repeat(BodyPropList, DefaultValue) ->
    Value = proplists:get_value(<<"backgroundRepeat">>, BodyPropList, DefaultValue),
    Value2 = sanitizer_util:sanitize_all(string_util:trim(Value), false, 10, <<"A-z0-9">>),
	ValidRepeats = [<<"no-repeat">>, <<"repeat">>],
	case lists:member(Value2, ValidRepeats) of
		true ->
			Value2;
		_ ->
			lists:nth(1, ValidRepeats)
	end.
