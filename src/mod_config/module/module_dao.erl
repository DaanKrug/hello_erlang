-module(module_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 user_is_owner_module/1,
		 module_active/1,
		 already_exists/2,
		 load_object_by_id/1, 
		 create_object/3, 
		 update_object/3, 
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

user_is_owner_module(UserId) ->
	Sql = "select id from module where ownerId = ? limit 1",
	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.

module_active(ModuleName) ->
	Sql = lists:concat([
						"select id from module where name = ? and active = true ",
						" and (deleted_at is null or deleted_at = '0000-00-00 00:00:00') limit 1"
						]),
	(base_dao:load_count(Sql, [ModuleName], ?MODULE)) > 0.

already_exists(Id, Name) ->
	Sql = lists:concat([
						"select id from module where id <> ? and name = ? ",
						" and (deleted_at is null or deleted_at = '0000-00-00 00:00:00') limit 1"
						]),
	(base_dao:load_count(Sql, [Id, Name], ?MODULE)) > 0.

load_object_by_id(Id) ->
	Sql = "select id, name, active, ownerId from module where id = ? limit 1",
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Name, Active, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "insert into module(name, active, ownerId, created_at) values (?,?,?,?)",
	ParamValues = [Name, boolean_util:to_integer(Active), OwnerId, CreatedAt],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Name, Active) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "update module set name = ?, active = ?, updated_at = ? where id = ?",
	ParamValues = [Name, boolean_util:to_integer(Active), UpdatedAt, Id],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = "select id, name, active, ownerId from module",
	OrderByCondition = " order by name asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = "select 0 as id, name, active, 0 as ownerId from module",
	OrderByCondition = " order by name asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

delete_object(Id) -> 
    Sql = "update module set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update module set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from module where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"active">>),
	IsNumber3 = string_util:equals(ColumnName, <<"ownerId">>),
	case (IsNumber or IsNumber2 or IsNumber3) of
		true ->
			"number";
		_ ->
			"binary"
	end.
