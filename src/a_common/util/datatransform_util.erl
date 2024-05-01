-module(datatransform_util).

-export(
   		[
		 is_undefined/1,
		 is_emptylist/1,
		 is_emptystring/1,
		 translate_and_validate/3, 
		 list_to_binary_format/1
		]
	   ).

is_undefined(Value) ->
	if
		(null == Value) -> true;
		(undefined == Value) -> true;
		true -> false
	end.

is_emptylist(List) ->
	IsUndefined = is_undefined(List),
	case IsUndefined of
		true ->
			true;
		_ ->
			length(List) == 0
	end.

is_emptystring(String) ->
	IsUndefined = is_undefined(String),
	case IsUndefined of
		true ->
			true;
		_ ->
			String2 = list_to_binary_format(String),
			case is_bitstring(String2) of
				true ->
					string:length(String2) == 0;
				_ ->
					true
			end
	end.
	
list_to_binary_format(Value) ->
	IsUndefined = is_undefined(Value),
	if
		IsUndefined -> <<"">>;
		(is_binary(Value)) -> Value;
		(is_list(Value)) -> iolist_to_binary(Value);
		true -> iolist_to_binary([Value])
	end.

translate_and_validate(Value, EnabledChars, IsNumeric) ->
	Graphemes = grapheme_util:to_graphemes(Value),
	IsEmptyGraphemes = is_emptylist(Graphemes),
	if
		(IsEmptyGraphemes) -> <<"">>;
		(IsNumeric) -> normalize_and_validate_graphemes(Graphemes, EnabledChars);
		true -> translate_and_validate_string(Graphemes, EnabledChars)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
translate_and_validate_string(Graphemes, EnabledChars) ->
	FromChars = char_util:strange_chars(),
	ToChars = char_util:translated_chars(),
	MaxLength = length(FromChars),
	Graphemes2 = translate_graphemes(Graphemes, FromChars, ToChars, 1, MaxLength),
	normalize_and_validate_graphemes(Graphemes2, EnabledChars).

translate_graphemes(Graphemes, FromChars, ToChars, Count, MaxLength) ->
	case ((Count > MaxLength)) of
		true -> 
			Graphemes;
		_ ->
			From = grapheme_util:get_grapheme_from_list(Count, FromChars),
			To = grapheme_util:get_grapheme_from_list(Count, ToChars),
			Graphemes2 = grapheme_util:replace_grapheme(Graphemes, From, To),
			translate_graphemes(Graphemes2, FromChars, ToChars, Count + 1, MaxLength)
	end.

normalize_and_validate_graphemes(Graphemes, EnabledChars) ->
	EnabledChars2 = struct_util:normalize_list(EnabledChars),
	case (struct_util:has_all_of_list_in_other(Graphemes, EnabledChars2)) of
		true ->
			iolist_to_binary(Graphemes);
		_ ->
			""
	end.