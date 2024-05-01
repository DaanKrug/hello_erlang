-module(boolean_util).

-export([to_integer/1]).

to_integer(Boolean) ->
	case Boolean of
		1 ->
			1;
		"1" -> 
			1;
		<<"1">> ->
			1;
		true ->
			1;
		"true" ->
			1;
		<<"true">> ->
			1;
		_ ->
			0
	end.