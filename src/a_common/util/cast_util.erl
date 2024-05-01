-module(cast_util).

-export([cast_to_type/2]).

cast_to_type(Value, Type) ->
	IsUndefined = (datatransform_util:is_undefined(Value) or datatransform_util:is_undefined(Type)),
	IsNumberType = string_util:equals(Type, <<"number">>),
	if
		(IsUndefined and IsNumberType) -> 0;
		(IsUndefined) -> <<" ">>;
		(IsNumberType) -> number_util:to_number(Value);
		true -> to_string(Value, Type)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_string(Value, Type) ->
	IsDate = string_util:equals(Type, <<"date">>),
	case IsDate of
		true ->
			case ((undefined == Value) or (Value == "")) of
				true ->
					<<"0000-00-00 00:00:00">>;
				_ ->
					Value2 = date_to_string(Value),
					datatransform_util:list_to_binary_format(Value2)
			end;
		_ -> 
			IsBinary = string_util:equals(Type, <<"binary">>),
			Value2 = basic_cast(Value, IsBinary),
			datatransform_util:list_to_binary_format(Value2)
	end.

basic_cast(Value, IsBinary) ->
	if 
		is_atom(Value) -> atom_to_binary(Value);
		is_number(Value) -> lists:concat(["", Value]);
		true -> 
			case (IsBinary and string_util:equals(Value, "")) of
				true ->
					<<" ">>;
				_ ->
					Value
			end
	end.

tuple_to_list_if_tuple(Value) -> 
	case is_tuple(Value) of
		true ->
			tuple_to_list(Value);
		_ -> 
			Value
	end.

date_to_string(Value) ->
	case is_tuple(Value) of
		true ->
			List = tuple_to_list_if_tuple(Value),
			Ymd = tuple_to_list_if_tuple(lists:nth(1, List)),
			His = tuple_to_list_if_tuple(lists:nth(2, List)),
			Year = string_util:left_zeros(lists:nth(1, Ymd), 4),
			Month = string_util:left_zeros(lists:nth(2, Ymd), 2),
			Day = string_util:left_zeros(lists:nth(3, Ymd), 2),
			Hour = string_util:left_zeros(lists:nth(1, His), 2),
			Minute = string_util:left_zeros(lists:nth(2, His), 2),
			Second = string_util:left_zeros(lists:nth(3, His), 2),
			lists:concat([Year, "-", Month, "-", Day, " ", Hour, ":", Minute, ":", Second]);
		_ -> 
			Value
	end.
