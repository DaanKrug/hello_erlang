-module(wallettransaction_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 load_object_by_id/1, 
		 create_object/5, 
		 update_object/2, 
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
						"select id, identifierSender, identifierReceiver, description, ",
						"amountCoins, status, ownerId from wallettransaction where id = ?"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(IdentifierSender, IdentifierReceiver, Description, AmountCoins, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into wallettransaction(identifierSender, identifierReceiver, ",
						"description, amountCoins, status, ownerId, created_at) ",
						"values (?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [
				   IdentifierSender, IdentifierReceiver, Description, 
				   AmountCoins, <<"pending">>, OwnerId, CreatedAt
				  ],
    Created = base_dao:create(Sql, ParamValues, ?MODULE),
	verify_after_create_object(Created).

update_object(Id, Status) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "update wallettransaction set status = ?, updated_at = ? where id = ?",
	ParamValues = [Status, UpdatedAt, Id],
	case (string_util:equals(Status, <<"commited">>)) of
		true ->
			Updated = base_dao:update(Sql, ParamValues, ?MODULE),
			confirm_commit_transaction(Id, Updated);
		_ ->
			case (string_util:equals(Status, <<"rolledback">>)) of
				true ->
					Updated = base_dao:update(Sql, ParamValues, ?MODULE),
					confirm_rollback_transaction(Id, Updated);
				_ ->
					false
			end
	end. 
	
load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, identifierSender, identifierReceiver, description, ",
						"amountCoins, status, ownerId, created_at, updated_at from wallettransaction"
					   ]),
	OrderByCondition = " order by id desc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, identifierSender, identifierReceiver, description, ",
						"amountCoins, status, ownerId, created_at, updated_at from wallettransaction"
					   ]),
	OrderByCondition = " order by id desc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

delete_object(Id) -> 
    Sql = "update wallettransaction set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update wallettransaction set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from wallettransaction where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"amountCoins">>),
	IsNumber3 = string_util:equals(ColumnName, <<"ownerId">>),
	IsDate = string_util:equals(ColumnName, <<"created_at">>),
	IsDate2 = string_util:equals(ColumnName, <<"updated_at">>),
	case (IsNumber or IsNumber2 or IsNumber3) of
		true ->
			"number";
		_ ->
			case (IsDate or IsDate2) of
				true ->
					"date";
				_ -> 
					"binary"
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verify_after_create_object(Created) ->
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Confirmated = confirm_begin_transaction(Id),
			case Confirmated of
				true ->
					Created;
				_ ->
					trully_delete_object(Id),
					[false, 0]
			end;
		_ ->
			Created
	end.

confirm_begin_transaction(Id) ->
	WalletTransaction = lists:nth(1, load_object_by_id(Id)),
	IdentifierSender = proplists:get_value(<<"identifierSender">>, WalletTransaction, ""),
	AmountCoins = proplists:get_value(<<"amountCoins">>, WalletTransaction, 0),
	case (AmountCoins > 0) of
		true ->
			wallet_dao:set_amount_coins(IdentifierSender, (AmountCoins * -1));
		_ ->
			false
	end.

confirm_commit_transaction(Id, Updated) ->
	case Updated of
		true ->
			WalletTransaction = load_object_by_id(Id),
			IdentifierReceiver = proplists:get_value(<<"identifierReceiver">>, WalletTransaction, ""),
			AmountCoins = proplists:get_value(<<"amountCoins">>, WalletTransaction, 0),
			case (AmountCoins > 0) of
				true ->
					Confirmated = wallet_dao:set_amount_coins(IdentifierReceiver, AmountCoins),
					case Confirmated of 
						true ->
							true;
						_ ->
							update_object(Id, <<"rolledback">>)
					end;
				_ ->
					false
			end;
		_ ->
			false
	end.

confirm_rollback_transaction(Id, Updated) ->
	case Updated of
		true ->
			WalletTransaction = load_object_by_id(Id),
			IdentifierSender = proplists:get_value(<<"identifierSender">>, WalletTransaction, ""),
			AmountCoins = proplists:get_value(<<"amountCoins">>, WalletTransaction, 0),
			case (AmountCoins > 0) of
				true ->
					wallet_dao:set_amount_coins(IdentifierSender, AmountCoins);
				_ ->
					false
			end;
		_ ->
			false
	end.
