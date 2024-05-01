-module(mailerconfig_handler).

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
	<<"Configura&ccedil;&atilde;o Envio E-mail">>.

get_base_url() -> 
	<<"/mailerconfigs">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>, <<"admin">>].

get_table_name() ->
	<<"mailerconfig">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	Provider = mailerconfig_dataextractor:get_provider(BodyPropList, ""),
    Name = mailerconfig_dataextractor:get_name(BodyPropList, ""),
    UserName = mailerconfig_dataextractor:get_user_name(BodyPropList, ""),
    Password = mailerconfig_dataextractor:get_password(BodyPropList, ""),
    PerMonth = mailerconfig_dataextractor:get_per_month(BodyPropList, 0),
    PerDay = mailerconfig_dataextractor:get_per_day(BodyPropList, 0),
    PerHour = mailerconfig_dataextractor:get_per_hour(BodyPropList, 0),
    PerMinute = mailerconfig_dataextractor:get_per_minute(BodyPropList, 0),
    PerSecond = mailerconfig_dataextractor:get_per_second(BodyPropList, 0),
    ReplayTo = mailerconfig_dataextractor:get_replay_to(BodyPropList, ""),
	Position = generic_dataextractor:get_position(BodyPropList, 0),
    UserId = user_dataextractor:get_user_id(BodyPropList),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	User = user_dao:load_object_by_id(UserId),
	UndefinedUser = datatransform_util:is_undefined(User),
    if
		UndefinedUser -> [messages_util:system_message(100065)];
        ((OwnerId < 1) or (PerMonth < 0)) -> [messages_util:system_message(412)];
		((PerDay < 0) or (PerHour < 0) or (Position < 1)) -> [messages_util:system_message(412)];
		((PerMinute < 0) or (PerSecond < 1)) -> [messages_util:system_message(412)];
		((Provider == "") or (Name == "") or (UserName == "")) -> [messages_util:system_message(100023)];
		((Password == "") or (ReplayTo == "")) -> [messages_util:system_message(100023)];
        true -> []
    end.

validate_to_update(BodyPropList, Id) -> 
	MailerConfig = lists:nth(1, edit(Id)),
	Provider = mailerconfig_dataextractor:get_provider(BodyPropList, 
													   proplists:get_value(<<"provider">>, MailerConfig, "")),
    Name = mailerconfig_dataextractor:get_name(BodyPropList, 
											   proplists:get_value(<<"name">>, MailerConfig, "")),
    UserName = mailerconfig_dataextractor:get_user_name(BodyPropList, 
														proplists:get_value(<<"username">>, MailerConfig, "")),
    Password = mailerconfig_dataextractor:get_password(BodyPropList, 
													   proplists:get_value(<<"password">>, MailerConfig, "")),
    PerMonth = mailerconfig_dataextractor:get_per_month(BodyPropList, 
														proplists:get_value(<<"perMonth">>, MailerConfig, 0)),
    PerDay = mailerconfig_dataextractor:get_per_day(BodyPropList, 
													proplists:get_value(<<"perDay">>, MailerConfig, 0)),
    PerHour = mailerconfig_dataextractor:get_per_hour(BodyPropList, 
													  proplists:get_value(<<"perHour">>, MailerConfig, 0)),
    PerMinute = mailerconfig_dataextractor:get_per_minute(BodyPropList, 
														  proplists:get_value(<<"perMinute">>, MailerConfig, 0)),
    PerSecond = mailerconfig_dataextractor:get_per_second(BodyPropList, 
														  proplists:get_value(<<"perSecond">>, MailerConfig, 0)),
    ReplayTo = mailerconfig_dataextractor:get_replay_to(BodyPropList, 
														proplists:get_value(<<"replayTo">>, MailerConfig, "")),
	Position = generic_dataextractor:get_position(BodyPropList, 
												  proplists:get_value(<<"position">>, MailerConfig, 0)),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	if
		((OwnerId < 1) or (Id < 1) or (PerMonth < 0)) -> [messages_util:system_message(412)];
		((PerDay < 0) or (PerHour < 0) or (Position < 1)) -> [messages_util:system_message(412)];
		((PerMinute < 0) or (PerSecond < 1)) -> [messages_util:system_message(412)];
		((Provider == "") or (Name == "") or (UserName == "")) -> [messages_util:system_message(100023)];
		((Password == "") or (ReplayTo == "")) -> [messages_util:system_message(100023)];
	    true -> []
    end.

validate_to_delete(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
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
	Provider = mailerconfig_dataextractor:get_provider(BodyPropList, ""),
    Name = mailerconfig_dataextractor:get_name(BodyPropList, ""),
    UserName = mailerconfig_dataextractor:get_user_name(BodyPropList, ""),
    Password = mailerconfig_dataextractor:get_password(BodyPropList, ""),
    PerMonth = mailerconfig_dataextractor:get_per_month(BodyPropList, 0),
    PerDay = mailerconfig_dataextractor:get_per_day(BodyPropList, 0),
    PerHour = mailerconfig_dataextractor:get_per_hour(BodyPropList, 0),
    PerMinute = mailerconfig_dataextractor:get_per_minute(BodyPropList, 0),
    PerSecond = mailerconfig_dataextractor:get_per_second(BodyPropList, 0),
    ReplayTo = mailerconfig_dataextractor:get_replay_to(BodyPropList, ""),
	Position = generic_dataextractor:get_position(BodyPropList, 0),
    UserId = user_dataextractor:get_user_id(BodyPropList),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	Created = mailerconfig_dao:create_object(Provider, Name, UserName, Password, Position, PerMonth, 
											 PerDay, PerHour, PerMinute, PerSecond, ReplayTo, UserId, OwnerId),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(200)];
		_ ->
			[messages_util:system_message(100024)]
	end.

update(BodyPropList, Id, OwnerId, Token) -> 
	MailerConfig = lists:nth(1, edit(Id)),
	Provider = mailerconfig_dataextractor:get_provider(BodyPropList, 
													   proplists:get_value(<<"provider">>, MailerConfig, "")),
    Name = mailerconfig_dataextractor:get_name(BodyPropList, 
											   proplists:get_value(<<"name">>, MailerConfig, "")),
    UserName = mailerconfig_dataextractor:get_user_name(BodyPropList, 
														proplists:get_value(<<"username">>, MailerConfig, "")),
    Password = mailerconfig_dataextractor:get_password(BodyPropList, 
													   proplists:get_value(<<"password">>, MailerConfig, "")),
    PerMonth = mailerconfig_dataextractor:get_per_month(BodyPropList, 
														proplists:get_value(<<"perMonth">>, MailerConfig, 0)),
    PerDay = mailerconfig_dataextractor:get_per_day(BodyPropList, 
													proplists:get_value(<<"perDay">>, MailerConfig, 0)),
    PerHour = mailerconfig_dataextractor:get_per_hour(BodyPropList, 
													  proplists:get_value(<<"perHour">>, MailerConfig, 0)),
    PerMinute = mailerconfig_dataextractor:get_per_minute(BodyPropList, 
														  proplists:get_value(<<"perMinute">>, MailerConfig, 0)),
    PerSecond = mailerconfig_dataextractor:get_per_second(BodyPropList, 
														  proplists:get_value(<<"perSecond">>, MailerConfig, 0)),
    ReplayTo = mailerconfig_dataextractor:get_replay_to(BodyPropList, 
														proplists:get_value(<<"replayTo">>, MailerConfig, "")),
	Position = generic_dataextractor:get_position(BodyPropList, 
												  proplists:get_value(<<"position">>, MailerConfig, 0)),
	Updated = mailerconfig_dao:update_object(Id, Provider, Name, UserName, Password, Position, PerMonth, 
											 PerDay, PerHour, PerMinute, PerSecond, ReplayTo),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, MailerConfig, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(100025)]
	end.

edit(Id) -> 
	mailerconfig_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	mailerconfig_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	mailerconfig_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case mailerconfig_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case mailerconfig_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case mailerconfig_dao:trully_delete_object(Id) of 
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

special_op1(_BodyPropList, _OwnerId, _Token) ->
	[messages_util:system_message(403)].

special_op2(_BodyPropList, _OwnerId, _Token) ->
	[messages_util:system_message(403)].
