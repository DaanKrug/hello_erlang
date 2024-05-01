-module(block_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 load_object_by_id/1, 
		 create_object/5, 
		 update_object/10, 
		 lock_block/1,
		 unlock_block/1,
		 load_all_objects/4, 
		 load_all_objects_for_public/4, 
		 delete_object/1, 
		 restore_object/1, 
		 trully_delete_object/1, 
		 get_column_type/1,
		 append_transaction_to_blockchain/1
		]
	   ).

get_database_name() ->
	lists:concat([base_dao:get_database_prefix_name(), "_coin"]).
	
load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, indexx, previous, time, keyy, hash, transactions, initiated, locked, ", 
						"mined_hash, mined_nonse, mined_key, miner_walletid, public_key, ", 
						"hash_signature from block where id = ?"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Index, Previous, Time, Key, Hash) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "insert into block(indexx, previous, time, keyy, hash, created_at) values (?,?,?,?,?,?)",
	ParamValues = [Index, Previous, Time, Key, Hash, CreatedAt],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Hash, Transactions, Initiated, MinedHash, MinedNonse, MinedKey, 
			  MinerWalletId, PublicKey, HashSignature) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update block set hash = ?, transactions = ?, initiated = ?, mined_hash = ?, ", 
                        "mined_nonse = ?, mined_key = ?, miner_walletid = ?, public_key = ?, ",
						"hash_signature = ?, updated_at = ? where id = ?"
					   ]),
	ParamValues = [
				   Hash, Transactions, 
				   boolean_util:to_integer(Initiated), 
				   MinedHash, MinedNonse, MinedKey, 
				   MinerWalletId, PublicKey, HashSignature, UpdatedAt, Id
				  ],
    base_dao:update(Sql, ParamValues, ?MODULE).

lock_block(BlockId) ->
    Sql = "update block set locked = true where id = ? and initiated = false",
    base_dao:update(Sql, [BlockId], ?MODULE).

unlock_block(BlockId) ->
	Sql = "update block set locked = false where id = ? and initiated = false",
    base_dao:update(Sql, [BlockId], ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, indexx, previous, time, keyy, hash, transactions, initiated, locked, ", 
						"mined_hash, mined_nonse, mined_key, miner_walletid, public_key, hash_signature from block"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, indexx, previous, time, keyy, hash, transactions, initiated, locked, ", 
						"mined_hash, mined_nonse, mined_key, miner_walletid, public_key, hash_signature from block"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

delete_object(Id) -> 
    Sql = "update block set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update block set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from block where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"indexx">>),
	IsNumber3 = string_util:equals(ColumnName, <<"time">>),
	IsNumber4 = string_util:equals(ColumnName, <<"mined_nonse">>),
	IsNumber5 = string_util:equals(ColumnName, <<"transactions">>),
	IsNumber6 = string_util:equals(ColumnName, <<"initiated">>),
	IsNumber7 = string_util:equals(ColumnName, <<"locked">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 or IsNumber5 or IsNumber6 or IsNumber7) of
		true ->
			"number";
		_ ->
			"binary"
	end.

append_transaction_to_blockchain(WalletTransaction) ->
	NextBlocks = load_next_to_append_transaction(),
	Block = lists:nth(1, NextBlocks),
	BlockId = proplists:get_value(<<"id">>, Block, 0),
	Locked = lock_block(BlockId),
	case Locked of
		true ->
			WalletTransactionId = proplists:get_value(<<"id">>, WalletTransaction, 0),
			Sender = proplists:get_value(<<"identifierSender">>, WalletTransaction, ""),
			Receiver = proplists:get_value(<<"identifierReceiver">>, WalletTransaction, ""),
			Amount = proplists:get_value(<<"amountCoins">>, WalletTransaction, 0),
			Created = transaction_dao:create_object(Sender, Receiver, Amount, WalletTransactionId, BlockId),
			case (lists:nth(1, Created)) of
				true ->
					update_block_hash(BlockId, Block),
					unlock_block(BlockId);
				_ ->
					false
			end;
		_ ->
			io:write([1]),
			append_transaction_to_blockchain(WalletTransaction)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_block_hash(BlockId, Block) ->
	Conditions = lists:concat([" and blockid = ", BlockId , " "]),
	Transactions = transaction_dao:load_all_objects(-1, -1, Conditions, false),
	Transactions2 = maplist_util:parse(Transactions),
	Time = proplists:get_value(<<"time">>, Block, 0),
	Previous = proplists:get_value(<<"previous">>, Block, ""),
	Index = proplists:get_value(<<"indexx">>, Block, 0),
	Key = proplists:get_value(<<"keyy">>, Block, ""),
	Hash = block:calculate_hash(Transactions2, Time, Previous, Index, Key),
	update_object(BlockId, Hash, length(Transactions), false, <<"">>, <<"">>, <<"">>, <<"">>, <<"">>, <<"">>).

load_next_to_append_transaction() ->
	Sql = lists:concat([
						"select id, indexx, previous, time, keyy from block where transactions < 10 ",
						"and time >= ? and initiated = false and locked = false order by id asc limit 1"
					   ]),
	Results = base_dao:load_by_params(Sql, [date_util:now(0) - 30000], ?MODULE),
	Result = lists:nth(1, Results),
	TotalRows = number_util:to_integer(proplists:get_value(<<"totalRows">>, Result, 0)),
	case (TotalRows == 0) of
		true ->
			Sql2 = "select id, indexx, previous, time, keyy from block order by id desc limit 1",
			Lasts = base_dao:load_by_params(Sql2, [], ?MODULE),
			Last = lists:nth(1, Lasts),
			TotalRows2 = number_util:to_integer(proplists:get_value(<<"totalRows">>, Last, 0)),
			Key = string_util:generate_random(100),
			case (TotalRows2 == 0) of
				true ->
					create_and_load_block(0, <<"">>, Key);
				_ -> 
					
					Index = number_util:to_integer(proplists:get_value(<<"indexx">>, Last, 0)),
					Previous = proplists:get_value(<<"hash">>, Last, ""),
					create_and_load_block(Index + 1, Previous, Key)
			end;
		_ ->
			Results
	end.

create_and_load_block(Index, Previous, Key) ->
	Created = create_object(Index, Previous, date_util:now(0), Key, <<"">>),
	case (lists:nth(1, Created)) of
		true ->
			load_object_by_id(lists:nth(2, Created));
		_ ->
			create_and_load_block(Index, Previous, Key)
	end.















