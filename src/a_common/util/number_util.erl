-module(number_util).

-export([
		 max_integer/0, 
		 is_nan/1, 
		 coalesce/3,
		 to_integer/1,
		 to_float/1,
		 to_float_format/3,
		 to_positive/1,
		 to_number/1,
		 coalesce_interval/3,
		 hex_to_decimal/1,
		 string_to_decimal/1,
		 concat_numbers/1,
		 integer_to_string/1,
		 float_to_string/1
		]
	   ).

-define(CHAR_DIGITS, [<<"0">>,<<"1">>,<<"2">>,<<"3">>,<<"4">>,<<"5">>,<<"6">>,<<"7">>,<<"8">>,<<"9">>]).

max_integer() ->
    4294967295.

is_nan(Value) ->
	case datatransform_util:is_undefined(Value) of
		true ->
			true;
		_ ->
			case (string_util:trim(Value) == "") of
				true ->
					true;
				_ -> 
					is_number(Value) == false
			end
	end.

coalesce(Value, DefaultValue, ZeroAsUndefined) ->
	case (is_nan(Value) or ((Value == 0) and ZeroAsUndefined)) of
		true -> 
			DefaultValue;
		_ -> 
			Value
	end.

to_integer(Value) ->
	case is_integer(Value) of
		true ->
			Value;
		_ ->
			Value1 = string_util:trim(Value),
			Value2 = string:replace(Value1, ",", ".", all), 
			concat_to_integer(string:split(Value2, "."))
	end.
	
to_float(Value) ->
	case is_float(Value) of
		true ->
			Value;
		_ ->
			Value1 = string_util:trim(Value),
			Value2 = string:replace(Value1, ",", ".", all), 
			Value3 = string:split(Value2, "."),
			case (length(Value3) > 1) of
			    true ->
					concat_to_float([lists:nth(1, Value3), ".", lists:nth(2, Value3)]);
				_ ->
					NormalizedList = struct_util:normalize_list(lists:nth(1, Value3)),
					concat_to_float([lists:nth(1, NormalizedList), ".0"])
			end
	end.

to_float_format(Value, Decimals, CommaAsDecimalSeparator) ->
    Value2 = string:split(lists:concat(["", to_float(Value)]), "."),
	Value3 = lists:nth(1,Value2),
	Value4 = lists:nth(2,Value2),
	Decimals2 = coalesce(Decimals, 2, false),
	Value5 = string_util:right_zeros(Value4, Decimals2),
	case CommaAsDecimalSeparator of
		true -> 
			lists:concat([Value3, ",", Value5]);
		_ -> 
			lists:concat([Value3, ".", Value5])
	end.

to_positive(Value) ->
	Value2 = to_float(Value),
	case (Value2 < 0) of
		true -> 
			Value2 * -1;
		_ -> 
			Value2
	end.

to_number(Value) ->
	case (is_integer(Value) or is_float(Value)) of
		true ->
			Value;
		_ ->
			Value2 = string:replace(Value, ",", ".", all),
			Value3 = string:find(Value2, "."),
			case Value3 of
				nomatch ->
					to_integer(Value2);
				_ ->
					to_float(Value2)
			end
	end.

coalesce_interval(Value, MinValue, MaxValue) ->
	case (is_nan(MinValue) or is_nan(MaxValue)) of
		true -> 
			to_number(Value);
		_ -> 
			less_than_or_great_than(to_number(Value), to_number(MinValue), to_number(MaxValue))
	end.
	
hex_to_decimal(HexString) ->
	case (is_binary(HexString) or is_bitstring(HexString)) of
		true ->
			HexString2 = binary_to_list(HexString),
			list_to_integer(HexString2, 16);
		_ ->
			list_to_integer(HexString, 16)
	end.
	
string_to_decimal(DecString) ->
	case (is_binary(DecString) or is_bitstring(DecString)) of
		true ->
			DecString2 = binary_to_list(DecString),
			list_to_integer(DecString2, 10);
		_ ->
			list_to_integer(DecString, 10)
	end.
	
concat_numbers(NumberList) ->
	NumberToStringFunction = fun (N) -> normalize_to_string(N) end,
	NumberStringsArray = lists:map(NumberToStringFunction,NumberList),
	array_util:concat_all(NumberStringsArray).
	
float_to_string(Float) ->
	case Float < 0 of
		true ->
			ResultArray = [<<"-">> | float_to_string2(Float * -1)],
			array_util:concat_all(ResultArray);
		_ ->
			ResultArray = float_to_string2(Float),
			array_util:concat_all(ResultArray)
	end.
	
integer_to_string(Integer) ->
	case Integer < 0 of
		true ->
			ResultArray = [<<"-">> | integer_to_string2(Integer * -1,[])],
			array_util:concat_all(ResultArray);
		_ -> 
			ResultArray = integer_to_string2(Integer,[]),
			array_util:concat_all(ResultArray)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

normalize_to_string(Value) ->
	if
		is_integer(Value) ->
			integer_to_string(Value);
		is_float(Value) -> 
			float_to_string(Value);
		true ->
			Value
	end.

float_to_string2(Float) ->
	Precision = math_util:get_precision(Float),
	List = float_to_list(Float, [{decimals, Precision}]),
	List2 = string_util:split(List,"."),
	[lists:nth(1,List2),<<".">>,lists:nth(2,List2)].

integer_to_string2(Integer,ResultArray) ->
	Integer2 = Integer div 10,
	Rest = Integer rem 10,
	ResultArray2 = [get_string_digit(Rest) | ResultArray],
	case Integer2 == 0 of
		true -> 
			ResultArray2;
		_ -> 
			integer_to_string2(Integer2,ResultArray2)
	end.
	
get_string_digit(Digit) ->
	lists:nth(Digit + 1,?CHAR_DIGITS).

concat_to_integer(List) ->
	{Number, _} = string:to_integer(lists:nth(1, List)),
	case Number of
		error -> 
			0;
		_ -> 
			Number
	end.

concat_to_float(List) ->
    List2 = lists:concat([List,""]),
	{Number, _} = string:to_float(List2),
	case Number of
		error -> 
			0.0;
		_ -> 
			Number
	end.

less_than_or_great_than(Value, MinValue, MaxValue) ->
	if
		(Value < MinValue) -> MinValue;
		(Value > MaxValue) -> MaxValue;
		true -> Value
	end.
