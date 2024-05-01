-module(transaction_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 load_object_by_id/1, 
		 create_object/5, 
		 update_object/4, 
		 load_all_objects/4, 
		 load_all_objects_for_public/4, 
		 delete_object/1, 
		 restore_object/1, 
		 trully_delete_object/1, 
		 get_column_type/1
		]
	   ).

get_database_name() ->
	lists:concat([base_dao:get_database_prefix_name(), "_coin"]).
	
load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, sender, receiver, amount, time, keyy, hash, wallettransactionid, ", 
						"blockid, miner_walletid, public_key, hash_signature from transaction where id = ?"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Sender, Receiver, Amount, WalletTransactionId, BlockId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
	Time = date_util:now(0),
	Key = string_util:generate_random(100),
	Hash = transaction:calculate_hash(Sender, Receiver, Amount, Time, Key),
    Sql = lists:concat([
						"insert into transaction(sender, receiver, amount, time, keyy, hash, ", 
						"wallettransactionid, blockid, created_at) values (?,?,?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [Sender, Receiver, Amount, Time, Key, Hash, WalletTransactionId, BlockId, CreatedAt],
    base_dao:create(Sql, ParamValues, ?MODULE).

% TODO: update the wallet transaction to finished
update_object(Id, MinerWalletId, PublicKey, HashSignature) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update transaction set miner_walletid = ?, public_key = ?, ", 
						"hash_signature = ?, updated_at = ? where id = ?"
					   ]),
	ParamValues = [MinerWalletId, PublicKey, HashSignature, UpdatedAt, Id],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, sender, receiver, amount, time, keyy, hash, ", 
						"wallettransactionid, blockid, miner_walletid, public_key, ",
						"hash_signature from transaction"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, sender, receiver, amount, time, keyy, hash, ", 
						"wallettransactionid, blockid, miner_walletid, public_key, ",
						"hash_signature from transaction"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

delete_object(Id) -> 
    Sql = "update transaction set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update transaction set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from transaction where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"amount">>),
	IsNumber3 = string_util:equals(ColumnName, <<"time">>),
	IsNumber4 = string_util:equals(ColumnName, <<"wallettransactionid">>),
	IsNumber5 = string_util:equals(ColumnName, <<"blockid">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 or IsNumber5) of
		true ->
			"number";
		_ ->
			"binary"
	end.
