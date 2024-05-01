-module(wallet_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 load_by_identifier/1,
		 load_by_recoverinfo/2,
		 load_object_by_id/1, 
		 create_object/4, 
		 update_object/3, 
		 set_login_fail/2,
		 set_amount_coins/2,
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
	
load_by_identifier(Identifier) ->
	Sql = "select id, identifier, password, active, loginFails, totalCoins, ownerId from wallet where identifier = ?",
	base_dao:load_by_params(Sql, [Identifier], ?MODULE).

load_by_recoverinfo(Identifier, RecoverInfo) ->
	Sql = "select id from wallet where identifier = ? and recoverinfo = ?",
	base_dao:load_by_params(Sql, [Identifier, RecoverInfo], ?MODULE).

load_object_by_id(Id) ->
	Sql = "select id, identifier, active, totalCoins, ownerId from wallet where id = ?",
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Identifier, Password, RecoverInfo, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "insert into wallet(identifier, password, recoverinfo, ownerId, created_at) values (?,?,?,?,?)",
	ParamValues = [Identifier, Password, RecoverInfo, OwnerId, CreatedAt],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Password, Active) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "update wallet set password = ?, active = ?, updated_at = ? where id = ?",
	ParamValues = [Password, boolean_util:to_integer(Active), UpdatedAt, Id],
    base_dao:update(Sql, ParamValues, ?MODULE).

set_login_fail(Id, LoginFails) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "update wallet set loginFails = ?, active = ?, updated_at = ? where id = ?",
	ParamValues = [LoginFails, boolean_util:to_integer((LoginFails < 3)), UpdatedAt, Id],
    base_dao:update(Sql, ParamValues, ?MODULE).

set_amount_coins(Identifier, AmountCoins) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "update wallet set totalCoins = (totalCoins + ?), updated_at = ? where identifier = ?",
	ParamValues = [AmountCoins, UpdatedAt, Identifier],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = "select id, identifier, active, loginFails, totalRewards, totalCoins, ownerId from wallet",
	OrderByCondition = " order by totalRewards desc, loginFails desc, totalCoins desc, identifier asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = "select id, identifier, active, totalCoins from wallet",
	OrderByCondition = " order by identifier asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

delete_object(Id) -> 
    Sql = "update wallet set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update wallet set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from wallet where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"active">>),
	IsNumber3 = string_util:equals(ColumnName, <<"ownerId">>),
	IsNumber4 = string_util:equals(ColumnName, <<"loginFails">>),
	IsNumber5 = string_util:equals(ColumnName, <<"totalCoins">>),
	IsNumber6 = string_util:equals(ColumnName, <<"totalRewards">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 or IsNumber5 or IsNumber6) of
		true ->
			"number";
		_ ->
			"binary"
	end.
