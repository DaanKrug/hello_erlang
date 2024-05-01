-module(baummuster_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 load_object_by_id/1, 
		 create_object/7, 
		 update_object/8, 
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
	
load_object_by_id(Id) ->
	Sql = lists:concat([
						"select userId, userName, userEmail, operation, objTitle, ",
						"ffrom ,tto, created_at, updated_at from applog where id = ?"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(UserId, UserName, UserEmail, Operation, ObjTitle, Ffrom, Tto) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into applog(userId, userName, userEmail, ",
						"operation, objTitle, ffrom, tto, created_at) ",
						"values (?,?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [UserId, UserName, UserEmail, Operation, ObjTitle, Ffrom, Tto, CreatedAt],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, UserId, UserName, UserEmail, Operation, ObjTitle, Ffrom, Tto) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update applog set userId = ?, userName = ?, userEmail = ?, ",
						"operation = ?, objTitle = ?, ffrom = ?, tto = ?, updated_at = ? ",
						"where id = ?"
					   ]),
	ParamValues = [UserId, UserName, UserEmail, Operation, ObjTitle, Ffrom, Tto, UpdatedAt, Id],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select userId, userName, userEmail, operation, objTitle, ",
						"ffrom ,tto, created_at, updated_at from applog"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select userId, userName, userEmail, operation, objTitle, ",
						"ffrom ,tto, created_at, updated_at from applog"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

delete_object(Id) -> 
    Sql = "update applog set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update applog set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from applog where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).


get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"userId">>),
	IsDate = string_util:equals(ColumnName, <<"created_at">>),
	IsDate2 = string_util:equals(ColumnName, <<"updated_at">>),
	IsDate3 = string_util:equals(ColumnName, <<"deleted_at">>),
	case IsNumber of
		true ->
			"number";
		_ ->
			case (IsDate or IsDate2 or IsDate3) of
				true ->
					"date";
				_ -> 
					"binary"
			end
	end.






























