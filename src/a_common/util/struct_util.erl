-module(struct_util).

-export(
   		[
		 normalize_list/1,
		 normalize_list_for_string/1,
		 has_empty/1, 
		 has_one_less_than/2, 
		 contained_in_one_of_list/2,
		 has_one_of_list_in_other/2, 
		 has_all_of_list_in_other/2,
		 get_value_from_list/2, 
		 replace_element/3
		]
	   ).

normalize_list(List) ->
	case (is_list(lists:nth(1, List))) of
		true ->
			lists:concat(List);
		_ ->
			List
	end.

normalize_list_for_string(List) ->
	if
		is_binary(List) -> binary_to_list(List);
		is_bitstring(List) -> binary_to_list(List);
		is_list(List) ->
			case (length(List) == 0) of
				true ->
					List;
				_ ->
					normalize_list(List)
			end;
		true -> List
	end.

has_empty(ArrayValues) ->
	has_empty(ArrayValues, 1).

has_one_less_than(ArrayValues, Value) ->
	has_one_less_than(ArrayValues, Value, 1).

contained_in_one_of_list(String, List) ->
	Undefined = datatransform_util:is_emptystring(String)
                or datatransform_util:is_emptylist(List),
	if
		Undefined -> false;
		true ->
		    contained_in_one_of_list2(String, List, 1, length(List))
	end.

has_one_of_list_in_other(List1, List2) ->
	Undefined = datatransform_util:is_emptylist(List1)
                or datatransform_util:is_emptylist(List2),
	if
		Undefined -> false;
		true -> has_one_of_list_in_other2(List1, List2, 1, length(List1))
	end.

has_all_of_list_in_other(List1, List2) ->
	Undefined = datatransform_util:is_emptylist(List1)
                or datatransform_util:is_emptylist(List2),
	if
		Undefined -> false;
		true -> has_all_of_list_in_other2(List1, List2, 1, length(List1))
	end.

get_value_from_list(Position, List) ->
	case (datatransform_util:is_emptylist(List)) of
		true ->
			undefined;
		_ ->
			case (Position > length(List)) of
				true ->
					undefined;
				_ ->
					lists:nth(Position, List)
			end
	end.

replace_element(List, Position, Value) ->
	Undefined = datatransform_util:is_emptylist(List)
                or datatransform_util:is_undefined(Position),
	if
		Undefined -> List;
		(Position < 1) -> List;
		(Position > length(List)) -> List;
		true -> % lists:sublist(List, Position) ++ [Value] ++ lists:nthtail(Position, List)
			lists:concat([lists:sublist(List, Position),[Value],lists:nthtail(Position, List)]) 
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
has_empty(ArrayValues, Count) ->
	case (Count > length(ArrayValues)) of
		true -> 
			false;
		_ ->
			Value = lists:nth(Count, ArrayValues),
			case (string_util:trim(Value) == "") of
				true -> 
					true;
				_ -> 
					has_empty(ArrayValues, Count + 1)
			end	
	end.

has_one_less_than(ArrayValues, Value, Count) ->
	case (Count > length(ArrayValues)) of
		true -> 
			false;
		_ ->
			Value2 = lists:nth(Count, ArrayValues),
			case Value2 < Value of
				true -> 
					true;
				_ -> 
					has_one_less_than(ArrayValues, Value, Count + 1)
			end	
	end.

contained_in_one_of_list2(String, List, Count, MaxLength) ->
	case (Count > MaxLength) of
		true ->
			false;
		_ ->
			String2 = lists:nth(Count, List),
			case (string:find(String2, String)) of
				nomatch ->
					contained_in_one_of_list2(String, List, Count + 1, MaxLength);
				_ ->
					true
			end
	end.

has_one_of_list_in_other2(List1, List2, Count, MaxLength) ->
	case (Count > MaxLength) of
		true ->
			false;
		_ ->
			Element = lists:nth(Count, List1),
			case (lists:member(Element, List2)) of
				true ->
					true;
				_ ->
					has_one_of_list_in_other2(List1, List2, Count + 1, MaxLength)
			end
	end.

has_all_of_list_in_other2(List1, List2, Count, MaxLength) ->
	case (Count > MaxLength) of
		true ->
			true;
		_ ->
			Element = lists:nth(Count, List1),
			case (lists:member(Element, List2) == false) of
				true ->
					false;
				_ ->
					has_all_of_list_in_other2(List1, List2, Count + 1, MaxLength)
			end
	end.

