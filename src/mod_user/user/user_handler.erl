-module(user_handler).

-behaviour(handler_behaviour).

-export([
		 init/2,
		 object_class_name/0,
		 get_base_url/0,
		 get_max_access_times_on_minute/0,
		 get_categories/0,
		 get_table_name/0,
		 authorize_operation/3,
		 validate_to_save/1,
		 validate_to_update/2,
		 validate_to_delete/1,
		 validate_to_restore/1,
		 validate_to_trully_delete/1,
		 save/3,
		 edit/1,
		 load_all/4,
		 load_all_for_public/4,
		 update/4,
		 delete/3,
		 restore/3,
		 trully_delete/3,
		 need_auth/2,
		 skip_validate_ownership/4,
		 authenticate/3,
		 logoff/2,
		 special_op1/3,
		 special_op2/3
		]).

init(Req, Opts) ->
	rest_handler:handle_request(Req, Opts, ?MODULE).

object_class_name() ->
	<<"Pessoa / Usu&aacute;rio">>.

get_base_url() -> 
	<<"/users">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>, <<"admin">>].

get_table_name() ->
	<<"user">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(BodyPropList, MethodToCall) ->
	case MethodToCall of 
		save -> 
			OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
			Id = generic_dataextractor:get_id(BodyPropList),
			((OwnerId == 0) and (Id == -1)) == false;
		_ ->
			true
	end.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	Id = generic_dataextractor:get_id(BodyPropList),
	Name = user_dataextractor:get_name(BodyPropList, ""),
    Email = user_dataextractor:get_email(BodyPropList, ""),
    Category = user_dataextractor:get_category(BodyPropList, ""),
    Password = user_dataextractor:get_password(BodyPropList),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	Id2 = user_dao:load_id_by_email(Email),
    if
    	((OwnerId < 1) and (Id >= 0)) -> [messages_util:system_message(412)];
		((Name == "") or (Email == "") or (Password == "") or (Category == "")) -> 
			[messages_util:system_message(100004)];
	    ((Id == 0) and (Id2 > 0)) -> [messages_util:system_message(100000,[Email])];
        ((Id == -1) and (Id2 > 0)) -> [messages_util:system_message(100019,[Email])];
	    (Category == <<"external">>) -> validate_save_external_user(OwnerId);
    	true -> []
    end.

validate_to_update(BodyPropList, Id) -> 
	User = user_dao:load_by_id(Id),
	Name = user_dataextractor:get_name(BodyPropList, proplists:get_value(<<"name">>, User, "")),
	Email = user_dataextractor:get_email(BodyPropList, proplists:get_value(<<"email">>, User, "")),
    Password0 = hash_util:hash_password(user_dataextractor:get_password(BodyPropList)),
	Password = string_util:coalesce(Password0, proplists:get_value(<<"password">>, User, "")),
	OldCategory = proplists:get_value(<<"category">>, User, ""),
    Category = user_dataextractor:get_category(BodyPropList, OldCategory),
	ChangedCategory = Category /= OldCategory,
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	Id2 = user_dao:load_id_by_email(Email),
	ValidateOwnerAdmin0 = lists:member(OldCategory, [<<"admin_master">>, <<"admin">>]),
	ValidateOwnerAdmin1 = lists:member(Category, [<<"admin_master">>, <<"admin">>]) == false,
	Undefined = datatransform_util:is_undefined(User),
	if
        ((Id < 1) or (OwnerId < 1) or Undefined) -> [messages_util:system_message(412)];
		((Name == "") or (Email == "") or (Password == "") or (Category == "")) -> 
			[messages_util:system_message(100004)];
	    ((Id2 > 0) and (Id /= Id2)) -> [messages_util:system_message(100000, [Email])];
		(ChangedCategory and (OldCategory == <<"admin_master">>)) -> [messages_util:system_message(100001)];
		(Category == <<"external">>) -> validate_save_external_user(OwnerId);
		(ValidateOwnerAdmin0 and ValidateOwnerAdmin1) -> validate_to_update_by_ownership(Id);
	    true -> []
    end.

validate_to_update_by_ownership(Id) ->
	IsOwner1 = user_dao:is_owner(Id),
	IsOwner2 = simplemail_dao:user_is_owner_simplemail(Id),
	IsOwner3 = pagemenuitemfile_dao:user_is_owner_pagemenuitemfile(Id),
    IsOwner4 = pagemenuitem_dao:user_is_owner_pagemenuitem(Id),
	IsOwner5 = pagemenu_dao:user_is_owner_pagemenu(Id),
    IsOwner6 = module_dao:user_is_owner_module(Id),
	IsOwner7 = mailerconfig_dao:user_is_owner_mailerconfig(Id),
	IsOwner8 = image_dao:user_is_owner_image(Id),
	IsOwner9 = file_dao:user_is_owner_file(Id),
    IsOwner10 = apptheme_dao:user_is_owner_apptheme(Id),
	IsOwner11 = appconfig_dao:user_is_owner_appconfig(Id),
	IsOwner12 = s3config_dao:user_is_owner_s3config(Id),
	if
		IsOwner1 -> [messages_util:system_message(100078)];
        IsOwner2 -> [messages_util:system_message(100082)];
        IsOwner3 -> [messages_util:system_message(100084)];
        IsOwner4 -> [messages_util:system_message(100086)];
        IsOwner5 -> [messages_util:system_message(100088)];
        IsOwner6 -> [messages_util:system_message(100090)];
        IsOwner7 -> [messages_util:system_message(100096)];
        IsOwner8 -> [messages_util:system_message(100098)];
        IsOwner9 -> [messages_util:system_message(100100)];
        IsOwner10 -> [messages_util:system_message(100102)];
        IsOwner11 -> [messages_util:system_message(100106)];
        IsOwner12 -> [messages_util:system_message(100128)];
        true -> []
    end.

validate_to_delete(Id) -> 
	User = user_dao:load_by_id(Id),
	Category = proplists:get_value(<<"category">>, User),
	ValidateOwnerAdmin = Category == <<"admin">>,
	Undefined = datatransform_util:is_undefined(User),
	if
        ((Id < 1) or Undefined) -> [messages_util:system_message(412)];
	    (Category == <<"admin_master">>) -> [messages_util:system_message(100003)];
	    ValidateOwnerAdmin -> validate_to_delete_by_ownership(Id);
	    true -> []
    end.

validate_to_delete_by_ownership(Id) ->
	IsOwner1 = user_dao:is_owner(Id),
	IsOwner2 = simplemail_dao:user_is_owner_simplemail(Id),
	IsOwner3 = pagemenuitemfile_dao:user_is_owner_pagemenuitemfile(Id),
	IsOwner4 = pagemenuitem_dao:user_is_owner_pagemenuitem(Id),
	IsOwner5 = pagemenu_dao:user_is_owner_pagemenu(Id),
	IsOwner6 = module_dao:user_is_owner_module(Id),
	IsOwner7 = mailerconfig_dao:user_is_owner_mailerconfig(Id),
	IsOwner8 = image_dao:user_is_owner_image(Id),
	IsOwner9 = file_dao:user_is_owner_file(Id),
    IsOwner10 = apptheme_dao:user_is_owner_apptheme(Id),
	IsOwner11 = appconfig_dao:user_is_owner_appconfig(Id),
    IsOwner12 = mailerconfig_dao:user_is_in_mailerconfig(Id),
	IsOwner13 = s3config_dao:user_is_owner_s3config(Id),
	if
		IsOwner1 -> [messages_util:system_message(100075)];
        IsOwner2 -> [messages_util:system_message(100083)];
        IsOwner3 -> [messages_util:system_message(100085)];
        IsOwner4 -> [messages_util:system_message(100087)];
        IsOwner5 -> [messages_util:system_message(100089)];
        IsOwner6 -> [messages_util:system_message(100091)];
        IsOwner7 -> [messages_util:system_message(100097)];
        IsOwner8 -> [messages_util:system_message(100099)];
        IsOwner9 -> [messages_util:system_message(100101)];
        IsOwner10 -> [messages_util:system_message(100103)];
        IsOwner11 -> [messages_util:system_message(100107)];
        IsOwner12 -> [messages_util:system_message(100116)];
        IsOwner13 -> [messages_util:system_message(100127)];
        true -> []
    end.

validate_to_restore(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

validate_to_trully_delete(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

save(BodyPropList, OwnerId, Token) -> 
	Id0 = generic_dataextractor:get_id(BodyPropList),
	Name = user_dataextractor:get_name(BodyPropList, ""),
    Email = user_dataextractor:get_email(BodyPropList, ""),
    Password = hash_util:hash_password(user_dataextractor:get_password(BodyPropList)),
    Category0 = user_dataextractor:get_category(BodyPropList, ""),
    Permissions = user_dataextractor:get_permissions(BodyPropList, ""),
    ConfirmationCode = string_util:generate_random(20),
    Active = generic_dataextractor:get_bool(BodyPropList, <<"active">>, false),   
    Category = get_category_for_save(OwnerId, generic_dataextractor:get_id(BodyPropList), Category0),
    Created = user_dao:create_object(Name, Email, Password, Category, Permissions, 
                                     ConfirmationCode, Active, OwnerId),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			User = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, User, object_class_name(), Token),
			if 
				((OwnerId == 0) and (Id0 == -1)) -> [mail_after_registered(Email, ConfirmationCode)];
				true -> [messages_util:system_message(200)]
			end;
		_ ->
			[messages_util:system_message(100005)]
	end.
  
update(BodyPropList, Id, OwnerId, Token) ->
	User = lists:nth(1, edit(Id)),
	Name = user_dataextractor:get_name(BodyPropList, ""),
    Email = user_dataextractor:get_email(BodyPropList, ""),
    Password0 = hash_util:hash_password(user_dataextractor:get_password(BodyPropList)),
	Password = string_util:coalesce(Password0, proplists:get_value(<<"password">>, User, "")),
	Category = user_dataextractor:get_category(BodyPropList, proplists:get_value(<<"category">>, User, "")),
    ConfirmationCode0 = user_dataextractor:get_confirmation_code(BodyPropList, 
											 proplists:get_value(<<"confirmation_code">>, User, "")),
	OldActive = (proplists:get_value(<<"active">>, User) == 1),
    Active = generic_dataextractor:get_bool(BodyPropList, <<"active">>, OldActive),
	Permissions0 = user_dataextractor:get_permissions(BodyPropList, ""),
	Permissions = get_permission_for_update(User, Active, OldActive, Permissions0),
	ConfirmationCode = get_confirmation_code_for_update(Active, ConfirmationCode0),
	Updated = user_dao:update_object(Id, Name, Email, Password, Category, Permissions, ConfirmationCode, Active),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, User, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(100007)]
	end.

edit(Id) -> 
	user_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	user_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	user_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case user_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case user_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case user_dao:trully_delete_object(Id) of 
		true ->
			applog_util:successfully_trully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(208)];
		_ ->
			[messages_util:system_message(500)]
	end.

authenticate(_BodyPropList, _Host, _Token) ->
	false.

logoff(_BodyPropList, _Token) ->
	false.

special_op1(BodyPropList, _OwnerId, Token) ->
	Id = generic_dataextractor:get_id(BodyPropList),
	[load_for_registering_or_login_logoff(BodyPropList, Id, Token)].

special_op2(_BodyPropList, _OwnerId, _Token) ->
	[messages_util:system_message(403)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_save_external_user(OwnerId) ->
	User = user_dao:load_by_id(OwnerId),
	Category = proplists:get_value(<<"category">>, User),
	case (Category == <<"admin_master">>) of
		true ->
			[messages_util:system_message(100147)];
		_ -> 
			[]
	end.

get_users_return(User, Password) ->
	case datatransform_util:is_undefined(User) of
		true ->
			[];
		_ -> 
			[{<<"password">>, Password} | proplists:delete(<<"password">>, User)]
	end.

get_hashed_password(User) ->
	case datatransform_util:is_undefined(User) of
		true ->
			undefined;
		_ -> 
			proplists:get_value(<<"password">>, User, "") 
	end.

get_user(Users) ->
	case datatransform_util:is_emptylist(Users) of
		true ->
			undefined;
		_ -> 
			lists:nth(1, Users)
	end.

get_category_for_save(OwnerId, Id, Category) ->
	case (((OwnerId == 0) and (Id == -1)) or (Category == "")) of
		true ->
			"enroll";
		_ ->
			Category
	end.

get_permission_for_update(User, Active, OldActive, Permissions) ->
	case (Active == OldActive) of
		true ->
			proplists:get_value(<<"permissions">>, User, "");
		_ ->
			Permissions
	end.

get_confirmation_code_for_update(Active, ConfirmationCode) ->
	if
		Active -> "";
		(ConfirmationCode == "") -> string_util:generate_random(20);
		true -> ConfirmationCode
	end.

mail_after_registered(Email, ConfirmationCode) ->
    case user_service:mail_on_registered(Email, ConfirmationCode) of
		true -> 
			messages_util:system_message(200);
		_ ->
			messages_util:system_message(100006)
	end.

load_for_registering_or_login_logoff(BodyPropList, Id, Token) ->
	ConfirmationCode = user_dataextractor:get_confirmation_code(BodyPropList, ""),
    Email = user_dataextractor:get_email(BodyPropList, ""),
    Password = user_dataextractor:get_password(BodyPropList),
    Users = user_dao:load_for_login_first_access_or_confirmation(Email, Password, ConfirmationCode),
    User = get_user(Users),
	HashedPassword = get_hashed_password(User),
	MatchedPassword = hash_util:password_match(HashedPassword, Password),
	Undefined = datatransform_util:is_undefined(User),
    if
      ((Id /= -1) or (Email == "") or (Token == "")) -> messages_util:system_message(412);
      Undefined -> [];
      MatchedPassword -> get_users_return(User, Password);
      true -> []
    end.
