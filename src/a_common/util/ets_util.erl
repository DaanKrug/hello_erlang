-module(ets_util).

-export([new/4, delete/1, store_in_cache/3, read_from_cache/2, remove_from_cache/2]).
      
new(SessionKey, Visibility, WriteConcurrency, ReadConcurrency) ->
	case ets_table_exists(SessionKey) of
		true ->
			SessionKey;
		_ ->
			Options = [
					   set, Visibility, named_table, 
                       {write_concurrency, WriteConcurrency}, 
					   {read_concurrency, ReadConcurrency}
					  ],
			ets:new(SessionKey, Options)
	end.

delete(SessionKey) ->
	case ets_table_exists(SessionKey) of
		true ->
			ets:delete(SessionKey);
		_ ->
			false
	end.

store_in_cache(SessionKey, Key, Value) ->
	case ets_table_exists(SessionKey) of
		true ->
			case remove_from_cache(SessionKey, Key) of
				true ->
					ets:insert(SessionKey, {Key, Value});
				_ -> 
					false
			end;
		_ -> 
			false
	end.

read_from_cache(SessionKey, Key) ->
	case ets_table_exists(SessionKey) of
		true ->
			Tuple = ets:lookup(SessionKey, Key),
			case is_tuple(Tuple) of
				true ->
					extract_lookup_tuple_value(Tuple);
				_ -> 
					extract_lookup_list_value(Tuple)
			end;
		_ ->
			undefined
	end.

remove_from_cache(SessionKey, Key) ->
	case ets_table_exists(SessionKey) of
		true ->
			ets:delete(SessionKey, Key),
			true;
		_ ->
			true
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ets_table_exists(SessionKey) ->
    ets:whereis(SessionKey) /= undefined.

extract_lookup_tuple_value(Tuple) ->
	lists:nth(2,tuple_to_list(Tuple)).

extract_lookup_list_value(Tuple) ->
	case (is_list(Tuple) and (length(Tuple) > 0)) of
		true ->
			extract_lookup_tuple_value(lists:nth(1, Tuple));
		_ -> 
			undefined
	end.

