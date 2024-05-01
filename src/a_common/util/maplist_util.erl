-module(maplist_util).

-export([parse/1]).

parse(PropList) ->
	Length = length(PropList),
	parse(PropList, [], 1, Length).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse(PropList, MapList, Count, Length) ->
	case (Count > Length) of
		true -> 
			lists:reverse(MapList);
		_ ->
			MapList2 = [maps:from_list(lists:nth(Count, PropList)) | MapList],
			parse(PropList, MapList2,  Count + 1, Length)
	end.







