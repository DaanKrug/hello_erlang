-module(grapheme_util).

-export([to_graphemes/1, replace_grapheme/3, get_grapheme_from_list/2]).

to_graphemes(String) ->
	case datatransform_util:is_emptystring(String) of
		true ->
			undefined;
		_ ->
			string:to_graphemes(String)
	end.

replace_grapheme(Graphemes, Grapheme, ReplacementGrapheme) ->
	Undefined = datatransform_util:is_undefined(Graphemes)
                or datatransform_util:is_undefined(Grapheme)
				or datatransform_util:is_undefined(ReplacementGrapheme),
	if
		Undefined -> Graphemes;
		true ->
			Length = length(Graphemes),
			replace_graphemes(Graphemes, [], Grapheme, ReplacementGrapheme, 1, Length)
	end.

get_grapheme_from_list(Position, GraphemesList) ->
	Grapheme = lists:nth(Position, GraphemesList),
	case (length(Grapheme) > 1) of
		true ->
			Grapheme;
		_ ->
			lists:nth(1, Grapheme)
	end.
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replace_graphemes(Graphemes, GraphemesNew, Grapheme, ReplacementGrapheme, Count, Length) ->
	case (Count > Length) of
		true ->
			lists:reverse(GraphemesNew);
		_ ->
			Grapheme1 = lists:nth(Count, Graphemes),
			GraphemesNew2 = case (Grapheme1 == Grapheme) of
								true ->
									[ReplacementGrapheme | GraphemesNew];
								_ ->
									[Grapheme1 | GraphemesNew]
							end,
			replace_graphemes(Graphemes, GraphemesNew2, Grapheme, ReplacementGrapheme, Count + 1, Length)
	end.
