-module(array_util).

-export([
    concat_all/1		 
]).
        
concat_all(ValuesArray) ->
	iolist_to_binary(lists:concat([ValuesArray,""])).