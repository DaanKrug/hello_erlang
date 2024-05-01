-module(cipherform_transmutter).

-export([cipher/1, decipher/1]).

-define(FACTOR, cipherform_util:get_factor()).
-define(RAND_HEADERS, cipherform_util:get_headers()).
-define(RAND_HEADERS_LENGTH, length(cipherform_util:get_headers())).
-define(VALID_CHARS, cipherform_util:valid_chars()).
-define(VALID_CHARS_LENGTH, length(cipherform_util:valid_chars())).

cipher(String) ->
    Chars = string:to_graphemes(String),
    Sum = string_util:left_zeros(rand:uniform(9999 - ?VALID_CHARS_LENGTH), 4),
    SumChars = string:to_graphemes(Sum),
    Header = cipher_chars(SumChars, [], 1, 4, ?FACTOR),
    Body = cipher_chars(Chars, [], 1, length(Chars), Sum),
	array_util:concat_all([
        lists:nth(rand:uniform(?RAND_HEADERS_LENGTH), ?RAND_HEADERS),
        Header,
        Body
    ]).

decipher(String) ->
    String2 = clear_one_header(String, 1, length(?RAND_HEADERS)),
    Header = string:slice(String2, 0, 16),
	Body = string:slice(String2, 16),
    HeaderA = extract_value_from_string_char(string:slice(Header, 0, 4)),
	HeaderB = extract_value_from_string_char(string:slice(Header, 4, 4)),
	HeaderC = extract_value_from_string_char(string:slice(Header, 8, 4)),
	HeaderD = extract_value_from_string_char(string:slice(Header, 12, 4)),
    SumList = [
        HeaderA - ?FACTOR,
        HeaderB - ?FACTOR,
        HeaderC - ?FACTOR,
        HeaderD - ?FACTOR
    ],
    SumString = number_util:concat_numbers(SumList),
    Sum = number_util:string_to_decimal(SumString),
    decipher_chars(Body,Sum, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cipher_chars(Chars, NewChars, Counter, Max, Sum) ->
	case (Counter > Max) of
		true ->
			array_util:concat_all(lists:reverse(NewChars));
		_ ->
			add_if_valid(Chars, NewChars, Counter, Max, Sum)
	end.

add_if_valid(Chars, NewChars, Counter, Max, Sum) ->
	Char = lists:nth(Counter, Chars),
    case lists:member(Char, ?VALID_CHARS) of
		true ->
			Ciphered = cipher_char(Char, Sum),
			NewChars2 = [Ciphered | NewChars],
			cipher_chars(Chars, NewChars2, Counter + 1, Max, Sum);
		_ ->
			cipher_chars(Chars, NewChars, Counter + 1, Max, Sum)
	end.

cipher_char(Char, Sum) ->
	Position = get_pos_char(Char, 1, ?VALID_CHARS_LENGTH),
	case (datatransform_util:is_undefined(Position)) of
		true ->
			"";
		_ ->
			String = string_util:left_zeros(integer_to_list(Sum + Position, 16), 4),
			Graphemes = string:to_graphemes(String),
			array_util:concat_all([
                lists:nth(2, Graphemes),
                lists:nth(1, Graphemes),
                lists:nth(4, Graphemes),
                lists:nth(3, Graphemes)
            ])
	end.

get_pos_char(Char, Counter, MaxLength) ->
	case (Counter > MaxLength) of
		true ->
			undefined;
		_ ->
			Char2 = lists:nth(Counter, ?VALID_CHARS),
			case (Char2 == Char) of
				true ->
					Counter;
				_ ->
					get_pos_char(Char, Counter + 1, MaxLength)
			end
	end.

clear_one_header(String, Counter, MaxLength) ->
	case (Counter > MaxLength) of
		true ->
			String;
		_ ->
			Header = lists:nth(Counter, ?RAND_HEADERS),
			Finded = string:find(String, Header),
			case ((nomatch /= Finded) and (Finded == String)) of
				true ->
					string:replace(String, Header, "");
				_ ->
					clear_one_header(String, Counter + 1, MaxLength)
			end
	end.

decipher_chars(String, Sum, DecipheredArray) ->
	case (datatransform_util:is_emptystring(String)) of
		true ->
			array_util:concat_all(lists:reverse(DecipheredArray));
		_ ->
			DecipheredChar = decipher_char(string:slice(String, 0, 4), Sum),
			DecipheredArray2 = [DecipheredChar | DecipheredArray],
			String2 = string:slice(String, 4),
			decipher_chars(String2, Sum, DecipheredArray2)
	end.

decipher_char(StringChar, Sum) ->
	Value = extract_value_from_string_char(StringChar),
    Position = math_util:remainder(Value - Sum,?VALID_CHARS_LENGTH),
    lists:nth(Position + 1, ?VALID_CHARS).


extract_value_from_string_char(StringChar) ->
	Graphemes = string:to_graphemes(StringChar),
    Reversed = [
        lists:nth(2, Graphemes),
        lists:nth(1, Graphemes),
        lists:nth(4, Graphemes),
        lists:nth(3, Graphemes)
    ],
    StringChar2 = array_util:concat_all(Reversed),
	number_util:hex_to_decimal(StringChar2).


