-module(pagemenu_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 user_is_owner_pagemenu/1,
		 load_page_menu_active_ids_for_public_menu/0,
		 object_exist/1,
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
	lists:concat([base_dao:get_database_prefix_name(), ""]).

user_is_owner_pagemenu(UserId) ->
	Sql = "select id from pagemenu where ownerId = ? limit 1",
  	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.

load_page_menu_active_ids_for_public_menu() ->
	Sql = lists:concat([
						"select GROUP_CONCAT(id) as ids from pagemenu where ",
						"active = true and onlyAuth = false ",
                        "and (deleted_at is null or deleted_at = '0000-00-00 00:00:00') "
					   ]),
	Result = base_dao:load_by_params(Sql, [], ?MODULE),
	case (datatransform_util:is_emptylist(Result)) of
		true ->
			"0";
		_ ->
			proplists:get_value(<<"ids">>, lists:nth(1, Result), "0")
	end.
	
object_exist(Id) ->
	Sql = "select id from pagemenu where id = ? limit 1",
  	(base_dao:load_count(Sql, [Id], ?MODULE)) > 0.

load_object_by_id(Id) ->
	Sql = "select id, name, position, active, onlyAuth, ownerId from pagemenu where id = ? limit 1",
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Name, Position, Active, OnlyAuth, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "insert into pagemenu(name, position, active, onlyAuth, ownerId, created_at) values (?,?,?,?,?,?)",
	ParamValues = [
				   Name, Position, boolean_util:to_integer(Active), 
				   boolean_util:to_integer(OnlyAuth), OwnerId, CreatedAt
				  ],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Name, Position, Active, OnlyAuth) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "update pagemenu set name = ?, position = ?, active = ?, onlyAuth = ?, updated_at = ? where id = ?",
	ParamValues = [
				   Name, Position, boolean_util:to_integer(Active), 
				   boolean_util:to_integer(OnlyAuth), UpdatedAt, Id
				  ],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = "select id, name, position, active, onlyAuth, ownerId from pagemenu",
	OrderByCondition = " order by position asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page, Rows, Conditions, FlagAsDeleted) -> 
	UsingMenus = module_dao:module_active(<<"menu">>),
	case (UsingMenus == true) of
		true ->
			Sql = "select id, name, position, active, onlyAuth, 0 as ownerId from pagemenu",
			OrderByCondition = " order by position asc ",
			base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE);
		_ ->
			[]
	end.
	
delete_object(Id) -> 
    Sql = "update pagemenu set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update pagemenu set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from pagemenu where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"position">>),
	IsNumber3 = string_util:equals(ColumnName, <<"active">>),
	IsNumber4 = string_util:equals(ColumnName, <<"ownerId">>),
	IsNumber5 = string_util:equals(ColumnName, <<"onlyAuth">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 or IsNumber5) of
		true ->
			"number";
		_ ->
			"binary"
	end.
