-module(file_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 user_is_owner_file/1,
		 object_exist/1,
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
	lists:concat([base_dao:get_database_prefix_name(), ""]).

user_is_owner_file(UserId) ->
  	Sql = "select id from file where ownerId = ? limit 1",
	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.

object_exist(Id) ->
	Sql = "select id from file where id = ? limit 1",
  	(base_dao:load_count(Sql, [Id], ?MODULE)) > 0.

load_object_by_id(Id) ->
	Sql = "select id, name, link, ownerId from file where id = ? limit 1",
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Name, Link, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "insert into file(name, link, ownerId, created_at) values (?,?,?,?)",
	ParamValues = [Name, Link, OwnerId, CreatedAt],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Name, Link) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = "update file set name = ?, link = ?, updated_at = ? where id = ?",
	ParamValues = [Name, Link, UpdatedAt, Id],
    Updated = base_dao:update(Sql, ParamValues, ?MODULE),
	case Updated of
		true ->
			pagemenuitemfile_dao:update_by_file(Id, Link),
			Updated;
		_ ->
			Updated
	end.

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = "select id, name, link, ownerId from file",
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(_Page, _Rows, _Conditions, _FlagAsDeleted) -> 
	[].

delete_object(Id) -> 
    Sql = "update file set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update file set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	File = load_object_by_id(Id),
	case datatransform_util:is_undefined(File) of 
		true ->
			false;
		_ ->
			% S3FileHandler.delete_from_s3(MapUtil.get(file,:link),MapUtil.get(file,:ownerId)))
			Sql = "delete from file where id = ?",
			base_dao:delete(Sql, [Id], ?MODULE)
	end.

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"ownerId">>),
	case (IsNumber or IsNumber2) of
		true ->
			"number";
		_ ->
			"binary"
	end.
