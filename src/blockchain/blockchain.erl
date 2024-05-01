-module(blockchain).

-export(
   		[
		 new/1, 
		 add_block/2, 
		 get_last_block/1, 
		 append_transaction/2, 
		 mine_pending_transactions/1
		]
	   ).

new(Map) ->
	Map2 = maps:put(<<"chain">>, [get_genesis_block()], Map),
	Map3 = maps:put(<<"pendind_transactions">>, [], Map2),
	Map4 = maps:put(<<"difficulty">>, 6, Map3),
	Map5 = maps:put(<<"miner_rewards">>, 50, Map4),
	maps:put(block_size, 10, Map5).

append_transaction(Map, Transaction) ->
	case transaction:is_valid_transaction(Transaction) of
		true ->
			PendingTransactions = maps:get(pendind_transactions, Map),
			PendingTransactions2 = [Transaction | lists:reverse(PendingTransactions)],
			maps:put(pendind_transactions, lists:reverse(PendingTransactions2), Map);
		_ ->
			Map
	end.

add_block(Map, NewBlock) ->
	Chain = maps:get(<<"chain">>, Map),
	LastBlock = get_last_block(Chain),
	Previous = get_last_block_hash(LastBlock),
	NewBlock2 = maps:put(<<"previous">>, Previous, NewBlock),	
	Chain2 = [NewBlock2 | lists:reverse(Chain)],
	maps:put(<<"chain">>, lists:reverse(Chain2), Map).

mine_pending_transactions(Map) ->
	PendingTransactions = maps:get(<<"pendind_transactions">>, Map),
	Length = length(PendingTransactions),
	case (Length == 0) of
		true ->
			Map;
		_ ->
			BlockSize = maps:get(block_size, Map),
			mine_pending_transactions2(Map, PendingTransactions, BlockSize, 1, Length)
	end.
				
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mine_pending_transactions2(Map, PendingTransactions, BlockSize, Count, Length) ->
	case (Count > Length) of
		true ->
			Map;
		_ ->
			Limit = Count + BlockSize,
			PendingTransactions2 = blockchain_util:split_transactions(PendingTransactions, [], Count, Limit, 1, Length),
			Chain = maps:get(<<"chain">>, Map),
			LastBlock = get_last_block(Chain),
			LastHash = get_last_block_hash(LastBlock),
			Difficulty = maps:get(<<"difficulty">>, Map),
			Index = length(Chain) + 1,
			Key = string_util:generate_random(Difficulty + 1),
			NewBlock = block:new(#{}, PendingTransactions2, date_util:now(0), Index, Key),
			NewBlock2 = maps:put(<<"previous">>, LastHash, NewBlock),
			NewBlock3 = block:mine_block(NewBlock2, Difficulty),
			Map2 = add_block(Map, NewBlock3),
			mine_pending_transactions2(Map2, PendingTransactions, BlockSize, Count + 1, Length)
	end.

get_genesis_block() ->
	Transaction0 = transaction:new(#{}, <<"anyone">>, <<"anotheranyone">>, 100, <<"initial_key">>, <<"anyone_t0">>),
	Transactions = [Transaction0],
	FixedTime = 1614866777145,
	block:new(#{}, Transactions, FixedTime, 1, <<"initial_key">>).

get_last_block(Chain) ->
	Size = length(Chain),
	case (Size == 0) of
		true ->
			undefined;
		_ ->
			lists:nth(Size, Chain)
	end.

get_last_block_hash(LastBlock) ->
	case (undefined == LastBlock) of
		true ->
			<<"none">>;
		_ ->
			maps:get(<<"hash">>, LastBlock)
	end.
