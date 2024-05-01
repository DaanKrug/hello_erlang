-module(simplemail_handler).

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
	<<"Email">>.

get_base_url() -> 
	<<"/simplemails">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>, <<"admin">>].

get_table_name() ->
	<<"simplemail">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	Subject = simplemail_dataextractor:get_subject(BodyPropList, ""),
    Content = simplemail_dataextractor:get_content(BodyPropList, ""),
	TosAddress = simplemail_dataextractor:get_tos_address(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
    if
      (OwnerId < 1) -> [messages_util:system_message(412)];
	  ((Subject == "") or (Content == "") or (TosAddress == "")) -> [messages_util:system_message(412)];
      true -> []
    end.


validate_to_update(BodyPropList, Id) -> 
	SimpleMail = lists:nth(1, simplemail_dao:load_object_by_id(Id)),
	OldSubject = proplists:get_value(<<"subject">>, SimpleMail, ""),
	OldContent = proplists:get_value(<<"content">>, SimpleMail, ""),
	OldTosAddress = proplists:get_value(<<"tosAddress">>, SimpleMail, ""),
	OldStatus = proplists:get_value(<<"status">>, SimpleMail, ""),
	Subject = simplemail_dataextractor:get_subject(BodyPropList, OldSubject),
    Content = simplemail_dataextractor:get_content(BodyPropList, OldContent),
	TosAddress = simplemail_dataextractor:get_tos_address(BodyPropList, OldTosAddress),
	Status = simplemail_dataextractor:get_status(BodyPropList, OldStatus),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	SuccessTotal = number_util:to_integer(proplists:get_value(<<"successTotal">>, SimpleMail)),
	InWork = lists:member(OldStatus, [<<"startProcessing">>, <<"processing">>]),
	Undefined = datatransform_util:is_undefined(SimpleMail),
	if
        ((Id < 1) or (OwnerId < 1) or Undefined) -> 
		    [messages_util:system_message(412)];
		((Subject == "") or (Content == "") or (TosAddress == "")) -> [messages_util:system_message(412)];
	    (InWork) -> 
		    [messages_util:system_message(100014)];
	    (SuccessTotal > 0) ->
		    case ((Subject /= OldSubject) or (Content /= OldContent) 
			      or (TosAddress /= OldTosAddress) or (Status /= "reSend")) of
				true ->
					[messages_util:system_message(100018)];
				_ ->
					[]
			end;
	    true -> []
    end.

validate_to_delete(Id) -> 
	SimpleMail = lists:nth(1, simplemail_dao:load_object_by_id(Id)),
	Status = proplists:get_value(<<"status">>, SimpleMail),
	InWork = lists:member(Status, [<<"startProcessing">>, <<"processing">>]),
	Undefined = datatransform_util:is_undefined(SimpleMail),
	if
      ((Id < 1) or Undefined) -> [messages_util:system_message(412)];
	  (InWork) -> [messages_util:system_message(100015)];
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
	Subject = simplemail_dataextractor:get_subject(BodyPropList, ""),
    Content = simplemail_dataextractor:get_content(BodyPropList, ""),
	TosAddress = simplemail_dataextractor:get_tos_address(BodyPropList, ""),
	RandomKey = generic_dataextractor:get_random_key(BodyPropList),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	Created = simplemail_dao:create_object(Subject, Content, TosAddress, RandomKey, OwnerId),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(200)];
		_ ->
			[messages_util:system_message(100016)]
	end.

update(BodyPropList, Id, OwnerId, Token) -> 
	SimpleMail = lists:nth(1, simplemail_dao:load_object_by_id(Id)),
	OldSubject = proplists:get_value(<<"subject">>, SimpleMail, ""),
	OldContent = proplists:get_value(<<"content">>, SimpleMail, ""),
	OldTosAddress = proplists:get_value(<<"tosAddress">>, SimpleMail, ""),
	OldStatus = proplists:get_value(<<"status">>, SimpleMail, ""),
	Subject = simplemail_dataextractor:get_subject(BodyPropList, OldSubject),
    Content = simplemail_dataextractor:get_content(BodyPropList, OldContent),
	TosAddress = simplemail_dataextractor:get_tos_address(BodyPropList, OldTosAddress),
	Status = simplemail_dataextractor:get_status(BodyPropList, OldStatus),
	Updated = simplemail_dao:update_object(Id, Subject, Content, TosAddress, Status),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, SimpleMail, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(100017)]
	end.

edit(Id) -> 
	simplemail_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	simplemail_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	simplemail_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case simplemail_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case simplemail_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case simplemail_dao:trully_delete_object(Id) of 
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
