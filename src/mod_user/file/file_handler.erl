-module(file_handler).

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
	<<"Arquivo">>.

get_base_url() -> 
	<<"/files">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>, <<"admin">>].

get_table_name() ->
	<<"file">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	Name = file_dataextractor:get_name(BodyPropList, ""),
    Link = file_dataextractor:get_link(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
    if
      (OwnerId < 1) -> [messages_util:system_message(412)];
	  ((Name == "") or (Link == "")) -> [messages_util:system_message(100026)];
      true -> []
    end.

validate_to_update(BodyPropList, Id) -> 
	File = lists:nth(1, edit(Id)),
	Name = file_dataextractor:get_name(BodyPropList, proplists:get_value(<<"name">>, File, "")),
    Link = file_dataextractor:get_link(BodyPropList, proplists:get_value(<<"link">>, File, "")),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	if
      ((Id < 1) or (OwnerId < 1)) -> [messages_util:system_message(412)];
	  ((Name == "") or (Link == "")) -> [messages_util:system_message(100026)];
	  true -> []
    end.

validate_to_delete(Id) -> 
	if
      (Id < 1) -> [messages_util:system_message(412)];
	  true -> 
		  case (pagemenuitemfile_dao:file_is_in_pagemenuitemfile(Id)) of
			  true -> 
				   [messages_util:system_message(100114)];
			  _ ->
				  []
		  end
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
	Name = file_dataextractor:get_name(BodyPropList, ""),
    Link = file_dataextractor:get_link(BodyPropList, ""),
	[save2(Name, Link, OwnerId, Token)].
	
update(BodyPropList, Id, OwnerId, Token) -> 
	File = lists:nth(1, edit(Id)),
	Name = file_dataextractor:get_name(BodyPropList, proplists:get_value(<<"name">>, File, "")),
    Link = file_dataextractor:get_link(BodyPropList, proplists:get_value(<<"link">>, File, "")),
	Updated = file_dao:update_object(Id, Name, Link),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, File, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(100028)]
	end.

edit(Id) -> 
	file_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	file_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	file_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case file_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case file_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case file_dao:trully_delete_object(Id) of 
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

special_op1(BodyPropList, OwnerId, Token) ->
	[upload_s3_and_save(BodyPropList, OwnerId, Token)].

special_op2(_BodyPropList, _OwnerId, _Token) ->
	[messages_util:system_message(403)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save2(Name, Link, OwnerId, Token) ->
	Created = file_dao:create_object(Name, Link, OwnerId),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			messages_util:system_message(200);
		_ ->
			messages_util:system_message(100027)
	end.

upload_s3_and_save(BodyPropList, OwnerId, Token) ->
    Name = file_dataextractor:get_name(BodyPropList, ""),
	FileName = file_dataextractor:get_file_name(BodyPropList, ""),
	FileBase64 = proplists:get_value(<<"file">>, BodyPropList, undefined),
	if
		(OwnerId < 1) -> messages_util:system_message(412);
		(Name == "") -> messages_util:system_message(100031);
		true -> 
			Link = validate_and_upload_s3(FileName, FileBase64, OwnerId),
			case datatransform_util:is_undefined(Link) of
				true ->
					messages_util:system_message(100027);
				_ ->
					save2(Name, Link, OwnerId, Token)
			end
	end.

validate_and_upload_s3(FileName, FileBase64, OwnerId) ->
	case ((FileName == "") or (FileBase64 == "")) of 
		true ->
			undefined;
		_ ->
			s3_file_handler:validate_and_upload_s3(FileName, FileBase64, OwnerId)
	end.

