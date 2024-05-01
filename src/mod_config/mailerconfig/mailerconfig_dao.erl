-module(mailerconfig_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 user_is_owner_mailerconfig/1,
		 user_is_in_mailerconfig/1,
		 load_object_by_id/1, 
		 create_object/13, 
		 update_object/12, 
		 load_all_objects/4, 
		 load_all_objects_for_public/4, 
		 delete_object/1, 
		 restore_object/1, 
		 trully_delete_object/1, 
		 get_column_type/1,
		 load_configs_for_mailing/0,
		 set_last_time_used/1
		]
	   ).

get_database_name() ->
	lists:concat([base_dao:get_database_prefix_name(), "_billingcontrol"]).

user_is_owner_mailerconfig(UserId) ->
	Sql = "select id from mailerconfig where ownerId = ? limit 1",
	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.

user_is_in_mailerconfig(UserId) ->
	Sql = "select id from mailerconfig where userId = ? limit 1",
	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.
  
load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, provider, name, username, password, position, ",
	                    "perMonth, perDay, perHour, perMinute, perSecond, replayTo, ",
                        "lastTimeUsed, userId, ownerId from mailerconfig where id = ? limit 1"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Provider, Name, UserName, Password, Position, PerMonth, PerDay, PerHour, 
			  PerMinute, PerSecond, ReplayTo, UserId, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into mailerconfig(provider, name, username, password, ",
                        "position, perMonth, perDay, perHour, perMinute, perSecond, replayTo, ",
						"userId, ownerId, created_at) values (?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [
				   Provider, Name, UserName, Password, Position, PerMonth, PerDay, PerHour, 
				   PerMinute, PerSecond, ReplayTo, UserId, OwnerId, CreatedAt
				  ],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Provider, Name, UserName, Password, Position, PerMonth, PerDay, 
			  PerHour, PerMinute, PerSecond, ReplayTo) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update mailerconfig set provider = ?, name = ?, username = ?, password = ?, ",
						"position = ?, perMonth = ?, perDay = ?, perHour = ?, perMinute = ?, perSecond = ?, ",
						"replayTo = ?, updated_at = ? where id = ?"
					   ]),
	ParamValues = [
				   Provider, Name, UserName, Password, Position, PerMonth, PerDay, PerHour, 
				   PerMinute, PerSecond, ReplayTo, UpdatedAt, Id
				  ],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, provider, name, username, password, position, perMonth, perDay, perHour, ",
						"perMinute, perSecond, replayTo, lastTimeUsed, userId, ownerId from mailerconfig"
					   ]),
	OrderByCondition = " order by position asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(_Page, _Rows, _Conditions, _FlagAsDeleted) -> 
	[].

delete_object(Id) -> 
    Sql = "update mailerconfig set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update mailerconfig set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from mailerconfig where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"position">>),
	IsNumber3 = string_util:equals(ColumnName, <<"perMonth">>),
	IsNumber4 = string_util:equals(ColumnName, <<"perDay">>),
	IsNumber5 = string_util:equals(ColumnName, <<"perHour">>),
	IsNumber6 = string_util:equals(ColumnName, <<"perMinute">>),
	IsNumber7 = string_util:equals(ColumnName, <<"perSecond">>),
	IsNumber8 = string_util:equals(ColumnName, <<"userId">>),
	IsNumber9 = string_util:equals(ColumnName, <<"ownerId">>),
	IsNumber10 = string_util:equals(ColumnName, <<"lastTimeUsed">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 or IsNumber5 
		  or IsNumber6 or IsNumber7 or IsNumber8 or IsNumber9 or IsNumber10) of
		true ->
			"number";
		_ ->
			"binary"
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    special functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_configs_for_mailing() ->
    Sql = lists:concat([
						"select id, provider, name, username, password, position, perMonth, perDay, perHour, ",
						"perMinute, perSecond, replayTo, lastTimeUsed, userId, ownerId from mailerconfig"
					   ]),
	OrderByCondition = " order by position asc ",
	base_dao:load_all(Sql, -1, -1, "", false, OrderByCondition, ?MODULE).

set_last_time_used(Id) ->
	Sql = "update mailerconfig set lastTimeUsed = ? where id = ?",
    Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

  









