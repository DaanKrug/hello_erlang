-module(itaubankconfig_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 load_object_by_id/1, 
		 create_object/5, 
		 update_object/5, 
		 load_all_objects/4, 
		 load_all_objects_for_public/4, 
		 delete_object/1, 
		 restore_object/1, 
		 trully_delete_object/1, 
		 get_column_type/1
		]
	   ).

get_database_name() ->
	lists:concat([base_dao:get_database_prefix_name(), "_config"]).

load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, a1_itaukey, a2_shoplinecode, a3_shoplinekey,",
						"a4_shoplinesite, ownerId from itaubankconfig where id = ? limit 1"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(A1Itaukey, A2Shoplinecode, A3Shoplinekey, A4Shoplinesite, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into itaubankconfig(a1_itaukey, a2_shoplinecode, a3_shoplinekey, ",
                        "a4_shoplinesite, ownerId, created_at) values (?,?,?,?,?,?)"
					   ]),
	ParamValues = [A1Itaukey, A2Shoplinecode, A3Shoplinekey, A4Shoplinesite, OwnerId, CreatedAt],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, A1Itaukey, A2Shoplinecode, A3Shoplinekey, A4Shoplinesite) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update itaubankconfig set a1_itaukey = ?, a2_shoplinecode = ?, ",
          				"a3_shoplinekey = ?, a4_shoplinesite = ?, updated_at = ? where id = ?"
					   ]),
	ParamValues = [A1Itaukey, A2Shoplinecode, A3Shoplinekey, A4Shoplinesite, UpdatedAt, Id],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						 "select id, a1_itaukey, a2_shoplinecode, a3_shoplinekey, ",
                         "a4_shoplinesite, ownerId from itaubankconfig"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page,Rows,Conditions,FlagAsDeleted) -> 
	Sql = lists:concat([
						 "select id, a1_itaukey, a2_shoplinecode, a3_shoplinekey, ",
                         "a4_shoplinesite, 0 as ownerId from itaubankconfig"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

delete_object(Id) -> 
    Sql = "update itaubankconfig set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update itaubankconfig set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from itaubankconfig where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"ownerId">>),
	case (IsNumber or IsNumber2) of
		true ->
			"number";
		_ ->
			"binary"
	end.
