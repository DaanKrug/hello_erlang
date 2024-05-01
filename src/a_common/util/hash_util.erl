-module(hash_util).

-export([hash_password/1, password_match/2]).

hash_password(Password) ->
	case (datatransform_util:is_emptystring(Password)) of
		true ->
			"";
		_ ->
		    Hash = crypto:hash(blake2b,Password),
		    EncodedHash = encode_graphemes(Hash),
			lists:concat([EncodedHash,""])
	end.

password_match(OriginalHashedPassword, ProvidedPassword) ->
	IsEmpty = datatransform_util:is_emptystring(OriginalHashedPassword)
			  or datatransform_util:is_emptystring(ProvidedPassword),
	if
		IsEmpty ->
			false;
		true ->
			NewHash = hash_password(ProvidedPassword),
            string_util:equals(NewHash,OriginalHashedPassword)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encode_graphemes(HashedPassword) ->
	Graphemes = binary_to_list(HashedPassword),
	calculate_graphemes(
		Graphemes,
		1,
		length(Graphemes),
		0
	).

calculate_graphemes(Graphemes,Position,Max,Total) ->
	if
		(Position >= Max) ->
			Total;
		true ->
			calculate_graphemes(
				Graphemes,
				Position + 1,
				Max,
				Total + calculate_grapheme(Graphemes,Position)
			)
	end.

calculate_grapheme(Graphemes,Position) ->
	Char = lists:nth(Position,Graphemes),
	Char * math_util:pow(2,Position).







