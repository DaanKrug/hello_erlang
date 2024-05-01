-module(appconfig_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 load_object_by_id/1, 
		 create_object/15, 
		 update_object/15, 
		 load_all_objects/4, 
		 load_all_objects_for_public/4, 
		 delete_object/1, 
		 restore_object/1, 
		 trully_delete_object/1, 
		 get_column_type/1,
		 already_has_active/1, 
		 user_is_owner_appconfig/1,
		 get_appconfig/0
		]
	   ).

get_database_name() ->
	lists:concat([base_dao:get_database_prefix_name(), "_config"]).

user_is_owner_appconfig(UserId) ->
	Sql = "select id from appconfig where ownerId = ? limit 1",
	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.
  	
already_has_active(Id) ->
    Sql = "select id from appconfig where id <> ? and active = true limit 1",
	(base_dao:load_count(Sql, [Id], ?MODULE)) > 0.

load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, name, description, site, usePricingPolicy, pricingPolicy, ",
						"usePrivacityPolicy, privacityPolicy, useUsetermsPolicy, usetermsPolicy, ",
						"useUsecontractPolicy, usecontractPolicy, useAuthorInfo, authorInfo, active, ownerId ",
						"from appconfig where id = ? limit 1"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Name, Description, Site, UsePricingPolicy, PricingPolicy, UsePrivacityPolicy,
			  PrivacityPolicy, UseUsetermsPolicy, UsetermsPolicy, UseUsecontractPolicy, UsecontractPolicy,
			  UseAuthorInfo, AuthorInfo, Active, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into appconfig(name, description, site, usePricingPolicy, pricingPolicy, ",
						"usePrivacityPolicy, privacityPolicy, useUsetermsPolicy, usetermsPolicy, ",
						"useUsecontractPolicy, usecontractPolicy, useAuthorInfo, authorInfo, ",
						"active, ownerId, created_at) values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [
				   Name, Description, Site, 
				   boolean_util:to_integer(UsePricingPolicy), PricingPolicy, 
				   boolean_util:to_integer(UsePrivacityPolicy), PrivacityPolicy, 
				   boolean_util:to_integer(UseUsetermsPolicy), UsetermsPolicy, 
				   boolean_util:to_integer(UseUsecontractPolicy), UsecontractPolicy,
			       boolean_util:to_integer(UseAuthorInfo), AuthorInfo, 
				   boolean_util:to_integer(Active), OwnerId, CreatedAt
				  ],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Name, Description, Site, UsePricingPolicy, PricingPolicy, UsePrivacityPolicy,
			  PrivacityPolicy, UseUsetermsPolicy, UsetermsPolicy, UseUsecontractPolicy, UsecontractPolicy,
			  UseAuthorInfo, AuthorInfo, Active) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update appconfig set name = ?, description = ?, site = ?, usePricingPolicy = ?, ",
						"pricingPolicy = ?, usePrivacityPolicy = ?, privacityPolicy = ?, useUsetermsPolicy = ?, ",
						"usetermsPolicy = ?, useUsecontractPolicy = ?, usecontractPolicy = ?, useAuthorInfo = ?, ",
						"authorInfo = ?, active = ?, updated_at = ? where id = ?"
					   ]),
	ParamValues = [
				   Name, Description, Site, 
				   boolean_util:to_integer(UsePricingPolicy), PricingPolicy, 
				   boolean_util:to_integer(UsePrivacityPolicy), PrivacityPolicy, 
				   boolean_util:to_integer(UseUsetermsPolicy), UsetermsPolicy, 
				   boolean_util:to_integer(UseUsecontractPolicy), UsecontractPolicy,
				   boolean_util:to_integer(UseAuthorInfo), AuthorInfo, 
				   boolean_util:to_integer(Active), UpdatedAt, Id
				  ],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, name, description, site, usePricingPolicy, pricingPolicy, ",
						"usePrivacityPolicy, privacityPolicy, useUsetermsPolicy, usetermsPolicy, ",
						"useUsecontractPolicy, usecontractPolicy, useAuthorInfo, authorInfo, ",
						"active, ownerId from appconfig"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(_Page, _Rows, _Conditions, FlagAsDeleted) -> 
	load_all_objects(1, 1, " and active = true ", FlagAsDeleted).

delete_object(Id) -> 
    Sql = "update appconfig set active = false, deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update appconfig set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from appconfig where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"usePricingPolicy">>),
	IsNumber3 = string_util:equals(ColumnName, <<"usePrivacityPolicy">>),
	IsNumber4 = string_util:equals(ColumnName, <<"useUsetermsPolicy">>),
	IsNumber5 = string_util:equals(ColumnName, <<"useUsecontractPolicy">>),
	IsNumber6 = string_util:equals(ColumnName, <<"useAuthorInfo">>),
	IsNumber7 = string_util:equals(ColumnName, <<"active">>),
	IsNumber8 = string_util:equals(ColumnName, <<"ownerId">>),
	case (IsNumber or IsNumber2 or IsNumber3 or IsNumber4 
		  or IsNumber5 or IsNumber6 or IsNumber7 or IsNumber8) of
		true ->
			"number";
		_ ->
			"binary"
	end.

get_appconfig() ->
	AppConfigs = load_all_objects_for_public(1, 1, "", false),
	case (datatransform_util:is_emptylist(AppConfigs)) of
		true ->
			undefined;
		_ ->
			lists:nth(1, AppConfigs)
	end.
