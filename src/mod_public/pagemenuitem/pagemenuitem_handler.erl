-module(pagemenuitem_handler).

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
	<<"Item Menu">>.

get_base_url() -> 
	<<"/pagemenuitems">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>, <<"admin">>].

get_table_name() ->
	<<"pagemenuitem">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	Name = pagemenu_dataextractor:get_name(BodyPropList, ""),
    Content = pagemenuitem_dataextractor:get_content(BodyPropList, ""),
    Position = generic_dataextractor:get_position(BodyPropList, 0),
    PageMenuId = pagemenuitem_dataextractor:get_page_menu_id(BodyPropList, 0),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
    if
        ((OwnerId < 1) or (Position < 1) or (PageMenuId < 1)) -> [messages_util:system_message(412)];
		((Name == "") or (Content == "")) -> [messages_util:system_message(100035)];
        true -> 
			case (pagemenu_dao:object_exist(PageMenuId)) of
				true ->
					[];
				_ ->
					[messages_util:system_message(100034)]
			end
    end.

validate_to_update(BodyPropList, Id) -> 
	PageMenuItem = lists:nth(1, edit(Id)),
	Name = pagemenu_dataextractor:get_name(BodyPropList, 
										   proplists:get_value(<<"name">>, PageMenuItem, "")),
    Content = pagemenuitem_dataextractor:get_content(BodyPropList,
													 proplists:get_value(<<"content">>, PageMenuItem, "")),
    Position = generic_dataextractor:get_position(BodyPropList, 
												  proplists:get_value(<<"position">>, PageMenuItem, 0)),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	if
        ((Id < 1) or (OwnerId < 1) or (Position < 1)) -> [messages_util:system_message(412)];
		((Name == "") or (Content == "")) -> [messages_util:system_message(100035)];
	    true -> []
    end.

validate_to_delete(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> 
			case (pagemenuitemfile_dao:pagemenuitem_is_in_pagemenuitemfile(Id)) of
				true ->
					[messages_util:system_message(100113)];
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
	Name = pagemenu_dataextractor:get_name(BodyPropList, ""),
    Content = pagemenuitem_dataextractor:get_content(BodyPropList, ""),
    Position = generic_dataextractor:get_position(BodyPropList, 0),
	Active = generic_dataextractor:get_bool(BodyPropList, <<"active">>, false),
    OnlyAuth = generic_dataextractor:get_bool(BodyPropList, <<"onlyAuth">>, false),
    PageMenuId = pagemenuitem_dataextractor:get_page_menu_id(BodyPropList, 0),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	Created = pagemenuitem_dao:create_object(Name, Content, Position, Active, 
											 OnlyAuth, PageMenuId, OwnerId),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(200)];
		_ ->
			[messages_util:system_message(100036)]
	end.

update(BodyPropList, Id, OwnerId, Token) -> 
	PageMenuItem = lists:nth(1, edit(Id)),
	Name = pagemenu_dataextractor:get_name(BodyPropList, 
										   proplists:get_value(<<"name">>, PageMenuItem, "")),
    Content = pagemenuitem_dataextractor:get_content(BodyPropList,
													 proplists:get_value(<<"content">>, PageMenuItem, "")),
    Position = generic_dataextractor:get_position(BodyPropList, 
												  proplists:get_value(<<"position">>, PageMenuItem, 0)),
	Active = generic_dataextractor:get_bool(BodyPropList, <<"active">>,
											proplists:get_value(<<"active">>, PageMenuItem, false)),
    OnlyAuth = generic_dataextractor:get_bool(BodyPropList, <<"onlyAuth">>,
											  proplists:get_value(<<"onlyAuth">>, PageMenuItem, false)),
    Updated = pagemenuitem_dao:update_object(Id, Name, Content, Position, Active, OnlyAuth),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, PageMenuItem, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(100037)]
	end.

edit(Id) -> 
	pagemenuitem_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	pagemenuitem_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	pagemenuitem_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case pagemenuitem_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case pagemenuitem_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case pagemenuitem_dao:trully_delete_object(Id) of 
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
