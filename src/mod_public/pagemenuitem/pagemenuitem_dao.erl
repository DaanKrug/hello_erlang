-module(pagemenuitem_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 user_is_owner_pagemenuitem/1,
		 page_menu_is_in_pagemenuitem/1,
		 object_exist/1,
		 load_object_by_id/1, 
		 create_object/7, 
		 update_object/6, 
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

user_is_owner_pagemenuitem(UserId) ->
	Sql = "select id from pagemenuitem where ownerId = ? limit 1",
  	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.
  
page_menu_is_in_pagemenuitem(PageMenuId) ->
    Sql = "select id from pagemenuitem where pageMenuId = ? limit 1",
  	(base_dao:load_count(Sql, [PageMenuId], ?MODULE)) > 0.

object_exist(Id) ->
	Sql = "select id from pagemenuitem where id = ? limit 1",
  	(base_dao:load_count(Sql, [Id], ?MODULE)) > 0.

load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, name, content, position, active, onlyAuth, ",
						"pageMenuId, ownerId from pagemenuitem where id = ? limit 1"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Name, Content, Position, Active, OnlyAuth, PageMenuId, OwnerId) -> 
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into pagemenuitem(name, content, position, active, onlyAuth, ",
						"pageMenuId, ownerId, created_at) values (?,?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [
				   Name, Content, Position, 
				   boolean_util:to_integer(Active), 
				   boolean_util:to_integer(OnlyAuth), 
				   PageMenuId, OwnerId, CreatedAt
				  ],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Name, Content, Position, Active, OnlyAuth) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update pagemenuitem set name = ?, content = ?, position = ?, ", 
						"active = ?, onlyAuth = ?, updated_at = ? where id = ?"
					   ]),
	ParamValues = [
				   Name, Content, Position, 
				   boolean_util:to_integer(Active), 
				   boolean_util:to_integer(OnlyAuth), 
				   UpdatedAt, Id
				  ],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = "select id, name, content, position, active,  onlyAuth, pageMenuId, ownerId from pagemenuitem",
	OrderByCondition = " order by position asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(Page, Rows, Conditions, FlagAsDeleted) -> 
	UsingMenus = module_dao:module_active(<<"menu">>),
	case (UsingMenus == true) of
		true ->
			Conditions2 = prepare_conditions_for_public(Conditions),
			Sql = lists:concat([
								"select id, name, content, position, active,  onlyAuth, ",
			                    " pageMenuId, 0 as ownerId from pagemenuitem"
							   ]),
			OrderByCondition = " order by position asc ",
			base_dao:load_all(Sql, Page, Rows, Conditions2, FlagAsDeleted, OrderByCondition, ?MODULE);
		_ ->
			[]
	end.

delete_object(Id) -> 
    Sql = "update pagemenuitem set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update pagemenuitem set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from pagemenuitem where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"position">>),
	IsNumber3 = string_util:equals(ColumnName, <<"active">>),
	IsNumber4 = string_util:equals(ColumnName, <<"onlyAuth">>),
	IsNumber5 = string_util:equals(ColumnName, <<"pageMenuId">>),
	IsNumber6 = string_util:equals(ColumnName, <<"ownerId">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 or IsNumber5 or IsNumber6) of
		true ->
			"number";
		_ ->
			"binary"
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prepare_conditions_for_public(Conditions) ->
	Conditions2 = string_util:to_string(Conditions),
	case (string:find(Conditions2, " pageMenuId in(")) of
		nomatch ->
			MenuIds = pagemenu_dao:load_page_menu_active_ids_for_public_menu(),
			lists:concat([
						  Conditions2,
						  " and onlyAuth = false and pageMenuId in(0",
						  string_util:to_string(MenuIds),
						  ") "
						 ]);
		_ ->
			Conditions2
	end.
