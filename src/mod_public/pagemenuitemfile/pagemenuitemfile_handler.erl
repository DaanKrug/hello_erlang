-module(pagemenuitemfile_handler).

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
	<<"Arquivo Item Menu">>.

get_base_url() -> 
	<<"/pagemenuitemfiles">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>, <<"admin">>].

get_table_name() ->
	<<"pagemenuitemfile">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	Position = generic_dataextractor:get_position(BodyPropList, 0),
    FileId = file_dataextractor:get_file_id(BodyPropList, 0),
    PageMenuItemId = pagemenuitemfile_dataextractor:get_page_menu_item_id(BodyPropList, 0),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
    if
        ((OwnerId < 1) or (FileId < 1)) -> [messages_util:system_message(412)];
		((PageMenuItemId < 1) or (Position < 0)) -> [messages_util:system_message(412)];
        true -> 
			case (file_dao:object_exist(FileId) /= true) of
				true ->
					[messages_util:system_message(100041)];
				_ ->
					case (pagemenuitem_dao:object_exist(PageMenuItemId) /= true) of
						true ->
							[messages_util:system_message(100038)];
						_ ->
							validate_already_exists(0, PageMenuItemId, FileId)
					end
			end
    end.

validate_to_update(_BodyPropList, _Id) -> 
	[messages_util:system_message(403)].

validate_to_delete(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

validate_to_restore(Id) -> 
	if
        (Id < 1) ->[ messages_util:system_message(412)];
	    true -> 
			PageMenuItemFile = lists:nth(1, edit(Id)),
			PageMenuItemId = proplists:get_value(<<"pageMenuItemId">>, PageMenuItemFile, 0),
			FileId = proplists:get_value(<<"fileId">>, PageMenuItemFile, 0),
			validate_already_exists(Id, PageMenuItemId, FileId)
    end.

validate_to_trully_delete(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

save(BodyPropList, OwnerId, Token) -> 
	Position = generic_dataextractor:get_position(BodyPropList, 0),
    FileId = file_dataextractor:get_file_id(BodyPropList, 0),
    PageMenuItemId = pagemenuitemfile_dataextractor:get_page_menu_item_id(BodyPropList, 0),
	File = lists:nth(1, file_dao:load_object_by_id(FileId)),
	Name = proplists:get_value(<<"name">>, File, ""),
	FileLink = proplists:get_value(<<"link">>, File, ""),
	Created = pagemenuitemfile_dao:create_object(Name, Position, FileId, FileLink, PageMenuItemId, OwnerId),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(200)];
		_ ->
			[messages_util:system_message(100039)]
	end.

update(_BodyPropList, _Id, _OwnerId, _Token) -> 
	[messages_util:system_message(403)].

edit(Id) -> 
	pagemenuitemfile_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	pagemenuitemfile_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	pagemenuitemfile_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case pagemenuitemfile_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case pagemenuitemfile_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case pagemenuitemfile_dao:trully_delete_object(Id) of 
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_already_exists(Id, PageMenuItemId, FileId) ->
	case (pagemenuitemfile_dao:already_exists(Id, PageMenuItemId, FileId)) of
		true ->
			[messages_util:system_message(100042)];
		_ ->
			[]
	end.