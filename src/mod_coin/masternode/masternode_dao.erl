-module(masternode_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 already_exists/2,
		 load_object_by_id/1, 
		 create_object/7, 
		 update_object/6, 
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

already_exists(Id, WalletId) ->
    Sql = "select id from masternode where id <> ? and a5_walletid = ? limit 1",
	(base_dao:load_count(Sql, [Id, WalletId], ?MODULE)) > 0.
	
load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, a1_name, a2_ip, a3_region, a4_master, a5_walletid, ", 
						"a6_walletidentifier, active, ownerId from masternode where id = ?"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(A1name, A2ip, A3region, A4master, A5walletid, A6walletidentifier, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into masternode(a1_name, a2_ip, a3_region, a4_master, a5_walletid, ",
						"a6_walletidentifier, active, ownerId, created_at) values (?,?,?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [
				   A1name, A2ip, A3region, 
				   boolean_util:to_integer(A4master), 
				   A5walletid, A6walletidentifier, 0, OwnerId, CreatedAt
				  ],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, A1name, A2ip, A3region, A4master, Active) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update masternode set a1_name = ?, a2_ip = ?, a3_region = ?, a4_master = ?, ",
						"active = ?, updated_at = ? where id = ?"
					   ]),
	ParamValues = [
				   A1name, A2ip, A3region, 
				   boolean_util:to_integer(A4master), 
				   boolean_util:to_integer(Active), 
				   UpdatedAt, Id
				  ],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, a1_name, a2_ip, a3_region, a4_master, a5_walletid, ", 
						"a6_walletidentifier, active, ownerId from masternode"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = "select a1_name, a2_ip, a3_region from masternode",
	OrderByCondition = " order by a3_region asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

delete_object(Id) -> 
    Sql = "update masternode set active = false, deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update masternode set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from masternode where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"a4_master">>),
	IsNumber3 = string_util:equals(ColumnName, <<"a5_walletid">>),
	IsNumber4 = string_util:equals(ColumnName, <<"active">>),
	IsNumber5 = string_util:equals(ColumnName, <<"ownerId">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 or IsNumber5) of
		true ->
			"number";
		_ ->
			"binary"
	end.
