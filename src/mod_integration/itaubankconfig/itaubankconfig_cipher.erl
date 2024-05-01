-module(itaubankconfig_cipher).

-export([cipher_core/2, convert/1]).

cipher_core(String1, String2) ->
    StringArray = initialize_arrays(String2),
    Graphemes = grapheme_util:to_graphemes(String1),
    cipher_core2(Graphemes, length(Graphemes), StringArray, [], 1, 1, 1).
  
convert(CipheredArray) ->
	RandomChar = string_util:generate_random_email(1),
	MaxLength = length(CipheredArray),
	convert2(CipheredArray, RandomChar, 1, MaxLength).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initialize_arrays(String) ->
    Graphemes = grapheme_util:to_graphemes(string:uppercase(String)),
    initialize_arrays2(Graphemes, length(Graphemes) + 1, [], [], 1).

initialize_arrays2(Graphemes, RemDivisor, KeyArray, StringArray, Counter) ->
	case (Counter > 256) of
		true ->
			initialize_arrays3(lists:reverse(KeyArray), lists:reverse(StringArray), 1, 1);
		_ ->
			KeyArray2 = [lists:nth((Counter rem RemDivisor), Graphemes) | KeyArray],
			StringArray2 = [Counter | StringArray],
			initialize_arrays2(Graphemes, RemDivisor, KeyArray2, StringArray2, Counter + 1)
	end.

initialize_arrays3(KeyArray, StringArray, K, Counter) ->
	case (Counter > 256) of
		true ->
			StringArray;
		_ ->
			calculate_k_and_replace(KeyArray, StringArray, K, Counter)
	end.

calculate_k_and_replace(KeyArray, StringArray, K, Counter) ->
	Ord = string_util:to_char_code(KeyArray, Counter),
    I = lists:nth(Counter, StringArray),
    K2 = (K + I + Ord) rem 257,
	StringArray2 = struct_util:replace_element(StringArray, Counter, lists:nth(K2, StringArray)),
    StringArray3 = struct_util:replace_element(StringArray2, K2, I),
	initialize_arrays3(KeyArray, StringArray3, K2, Counter + 1).

convert2(CipheredArray, Converted, Counter, MaxLength) ->
	case (Counter > MaxLength) of
		true ->
			Converted;
		_ ->
			RandomChar = string_util:generate_random_email(1),
			Converted2 = lists:concat([Converted, lists:nth(Counter, CipheredArray), RandomChar]),
			convert2(CipheredArray, Converted2, Counter + 1, MaxLength)
	end.
  
cipher_core2(Graphemes, MaxLength, StringArray, CipheredArray, K, M, Counter) ->
	case (Counter > MaxLength) of
		true ->
			lists:reverse(CipheredArray);
		_ ->
			cipher_core3(Graphemes, MaxLength, StringArray, CipheredArray, K, M, Counter)
	end.

cipher_core3(Graphemes, MaxLength, StringArray, CipheredArray, K, M, Counter) ->
	K2 = ((K + 1) rem 257),
	I = lists:nth(K2, StringArray),
    M2 = ((M + I) rem 257),
	StringArray2 = struct_util:replace_element(StringArray, K2, lists:nth(M2, StringArray)),
	StringArray3 = struct_util:replace_element(StringArray2, M2, I),
	Position = (lists:nth(K2, StringArray3) + lists:nth(M2, StringArray3)) rem 257,
	N = lists:nth(Position, StringArray3),
	Ord = string_util:to_char_code(Graphemes, Counter),
	OrdXorN = Ord xor N,
	OrdXorN2 = number_util:to_integer(OrdXorN),
    CipheredArray2 = [OrdXorN2 | CipheredArray],
	cipher_core2(Graphemes, MaxLength, StringArray3, CipheredArray2, K2, M2, Counter + 1).
