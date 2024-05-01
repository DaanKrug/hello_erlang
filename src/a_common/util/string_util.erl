-module(string_util).

-export([
		 to_string/1,
		 trim/1, 
		 remove_all_chars_in_string/2, 
		 generate_random/1, 
		 generate_random_only_num/1,
		 generate_random_filename/1,
		 generate_random_email/1,
		 capitalize/1,
		 left_zeros/2,
		 right_zeros/2,
		 left_spaces/2,
		 right_spaces/2,
		 truncate/2,
		 contains_one_of/2,
		 coalesce/2,
		 equals/2,
		 to_char_code/2,
		 to_list/1,
		 chunk_every/2,
		 split/2
		]).

to_string(Value) ->
	Undefined = datatransform_util:is_undefined(Value),
	if
		Undefined -> 
			<<>>;
		is_integer(Value) ->
			number_util:integer_to_string(Value);
		is_float(Value) -> 
			number_util:float_to_string(Value);
		is_list(Value) ->
			iolist_to_binary(Value);
		is_binary(Value) ->
			Value;
		is_bitstring(Value) and byte_size(Value) == 0 ->
			<<>>;
        is_bitstring(Value) ->
			Value;
		true -> <<>>
	end.

trim(Value) ->
	string:trim(to_string(Value)).
	
remove_all_chars_in_string(Value, CharsToRemove) ->
	remove_all_chars_in_string(to_string(Value), CharsToRemove, 1).

generate_random(Length) ->
	generate_random(Length, char_util:alpha_nums()).

generate_random_only_num(Length) ->
	generate_random(Length, char_util:only_nums()).

generate_random_filename(Length) ->
	generate_random(Length, char_util:filename_chars()).

generate_random_email(Length) ->
	generate_random(Length, char_util:email_chars()).

capitalize(Value) ->
	Value2 = trim(Value),
	case (Value2 /= "") of
		true -> 
			Array = string:split(Value2," ",all),
			capitalize_all(Array,[]);
		_ -> 
			""
	end.

left_zeros(Value, Length) ->
	left_chars([trim(Value)], "0", Length).

right_zeros(Value, Length) ->
	right_chars([trim(Value)], "0", Length).

left_spaces(Value, Length) ->
	left_chars([trim(Value)], " ", Length).

right_spaces(Value, Length) ->
	right_chars([trim(Value)], " ", Length).

truncate(Value, Length) ->
	string:slice(to_string(Value), 0, Length).
	
contains_one_of(Value, ArrayValues) ->
	Undefined1 = datatransform_util:is_emptystring(Value),
	Undefined2 = datatransform_util:is_emptylist(ArrayValues),
	if 
		Undefined1 -> false;
		Undefined2 -> false;
		true -> contains_one_of(Value, ArrayValues, 1)
	end.

coalesce(Value, ValueIfEmpty) ->
	Undefined = datatransform_util:is_emptystring(Value),
	if
		Undefined -> ValueIfEmpty;
		true -> Value
	end.

equals(Value1, Value2) ->
	to_string(Value1) == to_string(Value2).

to_char_code(Array, Position) ->
	Undefined = datatransform_util:is_emptylist(Array)
                or datatransform_util:is_undefined(Position),
	if
		Undefined -> undefined;
		(Position < 1) or (Position > length(Array)) -> undefined;
		true ->
			StringChar = lists:nth(Position, Array),
			case lists:member(StringChar, [<<"">>, undefined]) of
				true ->
					undefined;
				_ ->
					lists:nth(1,string:to_graphemes(trim(StringChar)))
			end
	end.

to_list(Value) ->
	case (is_binary(Value) or is_bitstring(Value)) of
		true ->
			binary_to_list(Value);
		_ ->
			Value
	end.
	
chunk_every(String,ChunkSize) ->
	chunk_every(String,ChunkSize,[]).
	
split(String,Separator) ->
	string:tokens(String,Separator).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chunk_every(String,ChunkSize,ResultArray) ->
	case (string:length(String) < ChunkSize) of
		true ->
			lists:reverse(ResultArray);
		_ ->
			chunk_every2(String,ChunkSize,ResultArray)
	end.

chunk_every2(String,ChunkSize,ResultArray) ->
	Chunk = string:slice(String, 0, ChunkSize),
	Rest = string:slice(String, ChunkSize),
	chunk_every(Rest,ChunkSize,[Chunk | ResultArray]).

remove_all_chars_in_string(Value, CharsToRemove, Count) ->
	case (Count > length(CharsToRemove)) of
		true -> 
			to_string(Value);
		_ -> 
			Char =  to_char_string(lists:nth(Count, CharsToRemove)),
			Value2 = string:replace(Value, Char, "", all),
			remove_all_chars_in_string(Value2, CharsToRemove, Count + 1)
	end.

to_char_string(Char) ->
	if
		is_binary(Char) or is_bitstring(Char) -> Char;
		is_number(Char) and ((Char > 255) or (Char < 0)) -> lists:concat([Char]);
		is_number(Char) -> to_string([Char]);
		is_list(Char) -> lists:concat([Char]);
		true -> Char
	end.

generate_random(MaxLength, Chars) ->
	case MaxLength < 1 of
		true -> 
			generate_random(10, Chars, "", length(Chars));
		_ ->
			generate_random(MaxLength, Chars, "", length(Chars))
	end.
	
generate_random(MaxLength, Chars, Result, RandLength) ->
	case string:length(Result) >= MaxLength of
		true ->
			Result;
		_ -> 
			Char = lists:nth(rand:uniform(RandLength), Chars),
			Result2 = lists:concat([Result, Char]),
			generate_random(MaxLength, Chars, Result2, RandLength)
	end.
	
capitalize_all(Array,Result) ->
	if
		(length(Array) == 0) and (length(Result) > 1) -> 
			to_string(lists:join(" ",lists:reverse(Result)));
		length(Array) == 0 -> 
			to_string(Result);
		length(Result) == 0 -> 
			Lower = string:lowercase(hd(Array)),
			Header = string:titlecase(Lower),
			capitalize_all(tl(Array),[Header]);
		true -> 
			Lower = string:lowercase(hd(Array)),
			Header = string:titlecase(Lower),
			capitalize_all(tl(Array),[Header | Result])
	end.

left_chars(Value, Char, Length) ->
	case (string:length(Value) >= Length) of
		true ->
			truncate(Value, Length);
		_ -> 
			left_chars(lists:concat([Char, Value]), Char, Length)
	end.

right_chars(Value, Char, Length) ->
	case (string:length(Value) >= Length) of
		true ->
			truncate(Value, Length);
		_ -> 
			right_chars(lists:concat([Value, Char]), Char, Length)
	end.

contains_one_of(Value, ArrayValues, Count) ->
	case (Count > length(ArrayValues)) of
		true ->
			false;
		_ -> 
			Char = to_char_string(lists:nth(Count, ArrayValues)),
			Match = string:find(Value, Char),
			case Match of
				nomatch ->
					contains_one_of(Value, ArrayValues, Count + 1);
				_ ->
					true
			end
	end.
