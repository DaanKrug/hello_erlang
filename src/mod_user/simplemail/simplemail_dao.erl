-module(simplemail_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 user_is_owner_simplemail/1,
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
	lists:concat([base_dao:get_database_prefix_name(), "_queue"]).

user_is_owner_simplemail(UserId) ->
	Sql = "select id from simplemail where ownerId = ? limit 1",
  	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.

load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, subject, content, tosAddress, successAddress, ", 
	                    "failAddress, status, tosTotal, successTotal, failTotal, ",
						"ownerId, updated_at from simplemail where id = ? limit 1"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Subject, Content, TosAddress, RandomKey, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
	Total = length(string_util:split(TosAddress, ",")),
    Sql = lists:concat([
						"insert into simplemail(subject, content, tosAddress, status, ",
                        "randomKey, ownerId, tosTotal, created_at) values (?,?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [Subject, Content, TosAddress, <<"awaiting">>, RandomKey, OwnerId, Total, CreatedAt],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Subject, Content, TosAddress, Status) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update simplemail set subject = ?, content = ?, tosAddress = ?, ",
						"status = ?, updated_at = ? where id = ?"
					   ]),
	ParamValues = [Subject, Content, TosAddress, Status, UpdatedAt, Id],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, subject, content, tosAddress, successAddress, ",
                        "failAddress, status, tosTotal, successTotal, failTotal, ownerId, ",
						"updated_at from simplemail "
					   ]),
	OrderByCondition = " order by id desc ",
	Conditions1 = lists:concat([Conditions, " and ownerId > 0 "]),
	base_dao:load_all(Sql, Page, Rows, Conditions1, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(_Page, _Rows, _Conditions, _FlagAsDeleted) -> 
	[].

delete_object(Id) -> 
    Sql = "update simplemail set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update simplemail set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from simplemail where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"tosTotal">>),
	IsNumber3 = string_util:equals(ColumnName, <<"successTotal">>),
    IsNumber4 = string_util:equals(ColumnName, <<"failTotal">>),
	IsNumber5 = string_util:equals(ColumnName, <<"ownerId">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 or IsNumber5) of
		true ->
			"number";
		_ ->
			"binary"
	end.
