-module(apptheme_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 user_is_owner_apptheme/1,
		 already_has_active/1,
		 load_object_by_id/1, 
		 create_object/13, 
		 update_object/13, 
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

user_is_owner_apptheme(UserId) ->
    Sql = "select id from apptheme where ownerId = ? limit 1",
  	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.

already_has_active(Id) ->
    Sql = lists:concat([
					    "select id from apptheme where ",
						"(deleted_at is null or deleted_at = '0000-00-00 00:00:00') ",
						" and id <> ? and active = true"
					   ]),
    (base_dao:load_count(Sql, [Id], ?MODULE)) > 0.

load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, name, image, defaultTheme, themes, organization, imageHeight, ",
						"positionX, positionY, backgroundWidth, backgroundHeight, backgroundRepeat, ",
						"active, ownerId from apptheme where id = ? limit 1"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Name, Image, DefaultTheme, Themes, Organization, ImageHeight, PositionX, PositionY,
			  BackgroundWidth, BackgroundHeight, BackgroundRepeat, Active, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into apptheme(name, image, defaultTheme, themes, organization, ",
						"imageHeight, positionX, positionY, backgroundWidth, backgroundHeight, ",
						"backgroundRepeat, active, ownerId, created_at) values (?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [
				   Name, Image, DefaultTheme, Themes, Organization, ImageHeight, 
				   PositionX, PositionY, BackgroundWidth, BackgroundHeight, BackgroundRepeat, 
				   boolean_util:to_integer(Active), OwnerId, CreatedAt
				  ],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Name, Image, DefaultTheme, Themes, Organization, ImageHeight, 
			  PositionX, PositionY, BackgroundWidth, BackgroundHeight, BackgroundRepeat, Active) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update apptheme set name = ?, image = ?, defaultTheme = ?, themes = ?, ",
						"organization = ?, imageHeight = ?, positionX = ?, positionY = ?, ",
						"backgroundWidth = ?, backgroundHeight = ?, backgroundRepeat = ?, active = ?, ",
						"updated_at = ? where id = ?"
					   ]),
	ParamValues = [
				   Name, Image, DefaultTheme, Themes, Organization, 
				   ImageHeight, PositionX, PositionY,
			       BackgroundWidth, BackgroundHeight, BackgroundRepeat, 
				   boolean_util:to_integer(Active), UpdatedAt, Id
				  ],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, name, image, defaultTheme, themes, organization, imageHeight, ", 
						"positionX, positionY, backgroundWidth, backgroundHeight, backgroundRepeat, ",
						"active, ownerId from apptheme"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(_Page, _Rows, _Conditions, _FlagAsDeleted) -> 
	load_all_objects(1, 1, " and active = true ", false).

delete_object(Id) -> 
    Sql = "update apptheme set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update apptheme set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from apptheme where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"imageHeight">>),
	IsNumber3 = string_util:equals(ColumnName, <<"backgroundWidth">>),
	IsNumber4 = string_util:equals(ColumnName, <<"backgroundHeight">>),
	IsNumber5 = string_util:equals(ColumnName, <<"active">>),
	IsNumber6 = string_util:equals(ColumnName, <<"ownerId">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 or IsNumber5 or IsNumber6) of
		true ->
			"number";
		_ ->
			"binary"
	end.
