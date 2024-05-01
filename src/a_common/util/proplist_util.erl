-module(proplist_util).

-export([put/4]).

put(Keys, Types, Values, PropList) ->
	Length = length(Keys),
	put(Keys, Types, Values, PropList, 1, Length).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
put(Keys, Types, Values, PropList, Count, Length) ->
	case (Count > Length) of
		true -> 
			PropList;
		_ ->
			Key = lists:nth(Count, Keys),
			Type = lists:nth(Count, Types),
			Value = struct_util:get_value_from_list(Count, Values),
			CastedValue = cast_util:cast_to_type(Value, Type),
			PropList2 = [{Key, CastedValue} | PropList],
			put(Keys, Types, Values, PropList2, Count + 1, Length)
	end.
	