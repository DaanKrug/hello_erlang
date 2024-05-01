-module(module_handler).

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
	<<"M&oacute;dulo">>.

get_base_url() -> 
	<<"/modules">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>].

get_table_name() ->
	<<"module">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	Name = module_dataextractor:get_name(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
    if
        (OwnerId < 1) -> [messages_util:system_message(412)];
		(Name == "") -> [messages_util:system_message(100031)];
        true -> 
			case (module_dao:already_exists(0, Name)) of
				true ->
					 [messages_util:system_message(100049)];
				_ ->
					[]
			end
    end.

validate_to_update(BodyPropList, Id) -> 
	Module = lists:nth(1, edit(Id)),
	Name = module_dataextractor:get_name(BodyPropList, proplists:get_value(<<"name">>, Module, "")),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	if
        ((Id < 1) or (OwnerId < 1)) -> [messages_util:system_message(412)];
		(Name == "") -> [messages_util:system_message(100031)];
		true -> 
			case (module_dao:already_exists(Id, Name)) of
				true ->
					 [messages_util:system_message(100049)];
				_ ->
					[]
			end
    end.

validate_to_delete(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

validate_to_restore(Id) -> 
	Module = lists:nth(1, edit(Id)),
	Name = proplists:get_value(<<"name">>, Module, ""),
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> 
			case (module_dao:already_exists(Id, Name)) of
				true ->
					 [messages_util:system_message(100049)];
				_ ->
					[]
			end
    end.

validate_to_trully_delete(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

save(BodyPropList, OwnerId, Token) -> 
	Name = module_dataextractor:get_name(BodyPropList, ""),
	Active = generic_dataextractor:get_bool(BodyPropList, <<"active">>, false),
	Created = module_dao:create_object(Name, Active, OwnerId),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(200)];
		_ ->
			[messages_util:system_message(100047)]
	end.

update(BodyPropList, Id, OwnerId, Token) -> 
	Module = lists:nth(1, edit(Id)),
	Name = module_dataextractor:get_name(BodyPropList, proplists:get_value(<<"name">>, Module, "")),
	Active = generic_dataextractor:get_bool(BodyPropList, <<"active">>, 
											proplists:get_value(<<"active">>, Module, false)),
	Updated = module_dao:update_object(Id, Name, Active),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, Module, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(100048)]
	end.

edit(Id) -> 
	module_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	module_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	module_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case module_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case module_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case module_dao:trully_delete_object(Id) of 
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
