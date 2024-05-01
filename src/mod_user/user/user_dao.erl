-module(user_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0, 
		 is_admin_master/1,
		 is_owner/1,
		 load_id_by_email/1,
		 load_admin_ids/0,
		 load_masteradmin_ids/0,
		 load_user_ids_of_owner/1,
		 create_if_dont_exists/3,
		 activate_user/1,
		 recover_user/1,
		 load_for_login_first_access_or_confirmation/3,
		 load_for_confirmation_code_no_password/1,
		 load_for_confirmation_code/2,
		 load_for_authentication/1,
		 load_for_activation/2,
		 load_for_recover/1,
		 load_for_edit/1,
		 load_object_by_id/1, 
		 create_object/8, 
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
	base_dao:get_database_prefix_name().

is_admin_master(Id) ->
    Sql = "select id from user where category = ? and id = ? limit 1",
    (base_dao:load_count(Sql, [<<"admin_master">>, Id], ?MODULE)) > 0.
  
is_owner(Id) ->
	Sql = "select id from user where ownerId = ? and id <> ? limit 1",
	(base_dao:load_count(Sql, [Id, Id], ?MODULE)) > 0.

load_id_by_email(Email) ->
	Sql = "select id from user where email = ? limit 1",
	base_dao:load_count(Sql, [Email], ?MODULE).

load_admin_ids() ->
    base_dao:load_by_params("select id from user where category = ?", [<<"admin">>], ?MODULE).

load_masteradmin_ids() ->
    base_dao:load_by_params("select id from user where category = ?", [<<"admin_master">>], ?MODULE).
  
load_user_ids_of_owner(UserId) ->
	Sql = "select GROUP_CONCAT(id) as ids from user where ownerId = ?",
    base_dao:load_by_params(Sql, [UserId], ?MODULE).
  
create_if_dont_exists(Email, FirstName, LastName) ->
    Owner = lists:nth(1, load_admin_ids()),
	OwnerId = proplists:get_value(<<"id">>, Owner),
    UserId = load_id_by_email(Email),
	case (UserId > 0) of
		true ->
			OwnerId;
		_ ->
			ConfirmationCode = string_util:generate_random(20),
			Password0 = string_util:generate_random(20),
			Password = hash_util:hash_password(Password0),
			Name = lists:concat([FirstName, " ", LastName]),
			create_object(Name, Email, Password, "enroll", "", ConfirmationCode, false, OwnerId),
			OwnerId
	end.
   
activate_user(Email) ->
	Sql = "update user set active = true, confirmation_code = null where email = ?",
	base_dao:update(Sql, [Email], ?MODULE).

recover_user(Email) ->
    ConfirmationCode = string_util:generate_random(20),
    Password = string_util:generate_random(20),
    Sql = "update user set active = false, password = ?, confirmation_code = ? where email = ?",
	ParamValues = [hash_util:hash_password(Password), ConfirmationCode, Email],
	Updated = base_dao:update(Sql, ParamValues, ?MODULE),
	case Updated of
		true ->
			user_service:mail_on_recovered(Email, Password, ConfirmationCode);
		_ ->
			false
	end.

load_for_login_first_access_or_confirmation(Email, Password, ConfirmationCode) ->
    if
        (Password == "") -> load_for_confirmation_code_no_password(Email);
        (ConfirmationCode /= "") -> load_for_confirmation_code(Email, ConfirmationCode);
        true -> load_for_authentication(Email)
    end.

load_for_confirmation_code_no_password(Email) ->
	Sql = lists:concat([
						"select email, confirmation_code from user where email = ? ",
						"and (deleted_at is null or deleted_at = '0000-00-00 00:00:00') ",
						"order by name asc limit 1"
					   ]),
    base_dao:load_by_params(Sql, [Email], ?MODULE).

load_for_confirmation_code(Email, ConfirmationCode) ->
	Sql = lists:concat([
						"select id, name, email, password, category, '' as permissions, ",
	                    "active, confirmation_code, 0 as ownerId from user where email = ? ",
						"and confirmation_code = ? "
						"and (deleted_at is null or deleted_at = '0000-00-00 00:00:00') ",
						"order by name asc limit 1"
					   ]),
    base_dao:load_by_params(Sql, [Email, ConfirmationCode], ?MODULE).
  
load_for_authentication(Email) ->
	Sql = lists:concat([
						"select id, name, email, password, category, permissions, ",
	                    "active, confirmation_code, 0 as ownerId from user where email = ? ",
						"and (deleted_at is null or deleted_at = '0000-00-00 00:00:00') ",
						"order by name asc limit 1"
					   ]),
    base_dao:load_by_params(Sql, [Email], ?MODULE).

load_for_activation(Email, ConfirmationCode) ->
	Sql = lists:concat([
						"select id, name, email, password, category, '' as permissions, ",
	                    "active, confirmation_code, ownerId from user where email = ? ",
						"and confirmation_code = ? ",
						"and (deleted_at is null or deleted_at = '0000-00-00 00:00:00') ",
						"order by name asc limit 1"
					   ]),
    base_dao:load_by_params(Sql, [Email, ConfirmationCode], ?MODULE).

load_for_recover(Email) ->
	Sql = lists:concat([
						"select id, name, email, password, category, '' as permissions, ",
	                    "active, confirmation_code, ownerId from user where email = ? ",
						"and (deleted_at is null or deleted_at = '0000-00-00 00:00:00') ",
						"order by name asc limit 1"
					   ]),
    base_dao:load_by_params(Sql, [Email], ?MODULE).

load_for_edit(Id) ->
	Sql = lists:concat([
						"select id, name, email, '' as password, category, permissions, ",
	                    "active, '' as confirmation_code, ownerId from user where id = ? limit 1"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).
  
load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, name, email, password, category, permissions, ", 
	                    "active, confirmation_code, ownerId from user where id = ? limit 1"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(Name, Email, Password, Category, Permissions, ConfirmationCode, Active, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into user(name, email, password, category, permissions, ",
	                    "confirmation_code, active, ownerId, created_at) ", 
	                    "values (?,?,?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [
				   Name, Email, Password, Category, Permissions, ConfirmationCode, 
				   boolean_util:to_integer(Active), OwnerId, CreatedAt
				  ],
    base_dao:create(Sql, ParamValues, ?MODULE).

update_object(Id, Name, Email, Password, Category, Permissions, ConfirmationCode, Active) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update user set name = ?, email = ?, password = ?, category = ?, ",
						"permissions = ?, confirmation_code = ?, active = ?, updated_at = ? where id = ?"
					   ]),
	ParamValues = [
				   Name, Email, Password, Category, Permissions, ConfirmationCode, 
				   boolean_util:to_integer(Active), UpdatedAt, Id
				  ],
    base_dao:update(Sql, ParamValues, ?MODULE).

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, name, email, '' as password, category, permissions, active, ",
                        "'' as confirmation_code, ownerId from user"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(_Page, _Rows, _Conditions, _FlagAsDeleted) -> 
	[].

delete_object(Id) -> 
    Sql = "update user set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update user set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from user where id = ?",
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