-module(baummuster_handler).

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
	<<"">>.

get_base_url() -> 
	<<"/s">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>].

get_table_name() ->
	<<"s">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	false.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
    if
        (OwnerId < 1) -> [messages_util:system_message(412)];
        true -> []
    end.

validate_to_update(BodyPropList, Id) -> 
	Id2 = number_util:to_integer(Id),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	if
        ((Id2 < 1) or (OwnerId < 1)) -> [messages_util:system_message(412)];
	    true -> []
    end.

validate_to_delete(Id) -> 
	Id2 = number_util:to_integer(Id),
	if
        (Id2 < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

validate_to_restore(Id) -> 
	Id2 = number_util:to_integer(Id),
	if
        (Id2 < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

validate_to_trully_delete(Id) -> 
	Id2 = number_util:to_integer(Id),
	if
        (Id2 < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

save(_BodyPropList, OwnerId, Token) -> 
	Created = [true, 0], %baummuster_dao:create_object(),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(200)];
		_ ->
			[messages_util:system_message(-1)]
	end.

update(_BodyPropList, Id, OwnerId, Token) -> 
	_Baummuster = lists:nth(1, edit(Id)),
	ObjectOld = lists:nth(1, edit(Id)),
	Updated = true, %baummuster_dao:update_object(),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, ObjectOld, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(-1)]
	end.

edit(Id) -> 
	baummuster_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	baummuster_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	baummuster_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case baummuster_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case baummuster_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case baummuster_dao:trully_delete_object(Id) of 
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
