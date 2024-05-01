-module(pagemenuitemfile_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 user_is_owner_pagemenuitemfile/1,
		 pagemenuitem_is_in_pagemenuitemfile/1,
		 file_is_in_pagemenuitemfile/1,
		 already_exists/3,
		 update_by_file/2,
		 load_object_by_id/1, 
		 create_object/6, 
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
	lists:concat([base_dao:get_database_prefix_name(), ""]).

user_is_owner_pagemenuitemfile(UserId) ->
	Sql = "select id from pagemenuitemfile where ownerId = ? limit 1",
  	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.

pagemenuitem_is_in_pagemenuitemfile(PageMenuItemId) ->
    Sql = "select id from pagemenuitemfile where pageMenuItemId = ? limit 1",
  	(base_dao:load_count(Sql, [PageMenuItemId], ?MODULE)) > 0.
  
file_is_in_pagemenuitemfile(FileId) ->
  	Sql = "select id from pagemenuitemfile where fileId = ? limit 1",
	(base_dao:load_count(Sql, [FileId], ?MODULE)) > 0.

already_exists(Id, PageMenuItemId, FileId) ->
	Sql = lists:concat([
						"select id from pagemenuitemfile where ",
						"(deleted_at is null or deleted_at = '0000-00-00 00:00:00') ",
						"and id <> ? and pageMenuItemId = ? and fileId = ?"
						]),
    (base_dao:load_count(Sql, [Id, PageMenuItemId, FileId], ?MODULE)) > 0.

update_by_file(FileId, FileLink) ->
    Sql = "update pagemenuitemfile set fileLink = ? where fileId = ?",
	base_dao:update(Sql, [FileId, FileLink], ?MODULE).

load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, name, position, fileId, fileLink, pageMenuItemId, ", 
	                    "ownerId from pagemenuitemfile where id = ? limit 1"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Name, Position, FileId, FileLink, PageMenuItemId, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into pagemenuitemfile(name, position, fileId, fileLink, ",
						"pageMenuItemId, ownerId, created_at) values (?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [Name, Position, FileId, FileLink, PageMenuItemId, OwnerId, CreatedAt],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Name, Position) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "update pagemenuitemfile set name = ?, position = ?, updated_at = ? where id = ?",
	ParamValues = [Name, Position, UpdatedAt, Id],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = "select id, name, position, fileId, fileLink, pageMenuItemId, ownerId from pagemenuitemfile",
	OrderByCondition = " order by position asc, name asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = "select id, name, position, fileId, fileLink, pageMenuItemId, 0 as ownerId from pagemenuitemfile",
	OrderByCondition = " order by position asc, name asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

delete_object(Id) -> 
    Sql = "update pagemenuitemfile set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update pagemenuitemfile set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from pagemenuitemfile where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) -> 
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"position">>),
	IsNumber3 = string_util:equals(ColumnName, <<"fileId">>),
	IsNumber4 = string_util:equals(ColumnName, <<"pageMenuItemId">>),
	IsNumber5 = string_util:equals(ColumnName, <<"ownerId">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 or IsNumber5) of
		true ->
			"number";
		_ ->
			"binary"
	end.
