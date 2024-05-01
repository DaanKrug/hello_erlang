-module(block).

-export([new/5, mine_block/2, calculate_hash/5]).

new(Map, Transactions, Time, Index, Key) ->
	Previous = <<"none">>,
	Map2 = maps:put(<<"previous">>, Previous, Map),
	Map3 = maps:put(<<"indexx">>, Index, Map2),
	Map4 = maps:put(<<"transactions">>, Transactions, Map3),
	Map5 = maps:put(<<"time">>, Time, Map4),
	maps:put(<<"hash">>, calculate_hash(Transactions, Time, Previous, Index, Key), Map5).

mine_block(Map, Difficulty) ->
	ArrayDifficulty = blockchain_util:full_array_int(Difficulty),
	HashPuzzle = lists:concat(ArrayDifficulty),
	Transactions = maps:get(transactions, Map),
	Time = maps:get(<<"time">>, Map),
	Previous = maps:get(<<"previous">>, Map),
	Index = maps:get(<<"indexx">>, Map),
	RandomLenght = 2 * length(blockchain_util:to_concat(HashPuzzle)),
	[MinedHash, MinedNonse, MinedKey] = mine_nonse_hash(HashPuzzle, 1, <<"">>, undefined, Transactions, 
									                    Time, Previous, Index, RandomLenght),
	Map2 = maps:put(<<"mined_hash">>, MinedHash, Map),
	Map3 = maps:put(<<"mined_nonse">>, MinedNonse, Map2),
	maps:put(<<"mined_key">>, MinedKey, Map3).

calculate_hash(Transactions, Time, Previous, Index, Key) ->
	SummHash = sum_hash(Transactions, <<"">>),
	HashString = lists:concat([
							   Time, 
							   blockchain_util:to_concat(SummHash), 
							   blockchain_util:to_concat(Previous), 
							   Index
							  ]),
	HashEncoded = blockchain_util:encode_hashstring(HashString),
	blockchain_util:crypto256Hex(Key, HashEncoded).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mine_nonse_hash(HashPuzzle, Nonse, Hash, Key, Transactions, Time, Previous, Index, RandomLenght) ->
	case (match_hash(Hash, HashPuzzle)) of
		false ->
			Key2 = string_util:generate_random(RandomLenght),
			Hash2 = calculate_hash(Transactions, Time, Previous, Index, Key2),
			mine_nonse_hash(HashPuzzle, Nonse + 1, Hash2, Key2, Transactions, 
							Time, Previous, Index, RandomLenght);
		_ ->
			io:format("\nMined whit nonse: ~p", [Nonse]),
			[Hash, Nonse, Key]
	end.

match_hash(Hash, HashPuzzle) ->
	HashList = blockchain_util:to_concat(Hash),
	HashHex = parse_hex_list(HashList, [], 1, length(HashList)),
	case (string:find(HashHex, HashPuzzle)) of
		nomatch ->
			false;
		_ ->
			true
	end.

parse_hex_list(HashList, HashHexList, Counter, Length) ->
	case (Counter > Length) of
		true ->
			lists:reverse(HashHexList);
		_ ->
			HashHexList2 = [integer_to_list(lists:nth(Counter, HashList), 16) | HashHexList],
			parse_hex_list(HashList, HashHexList2, Counter + 1, Length)
	end.

sum_hash(Transactions, Hash) ->
	case ((undefined == Transactions) or (length(Transactions) == 0)) of
		true ->
			Hash;
		_ ->
			[Header | Tail] = Transactions,
			Hash2 =  lists:concat([
								   blockchain_util:to_concat(Hash),
								   blockchain_util:to_concat(maps:get(<<"hash">>, Header))
								   ]),
			sum_hash(Tail, Hash2)
	end.
	








