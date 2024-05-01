-module(appconfig_handler).

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
	<<"Configura&ccedil;&atilde;o da Aplica&ccedil;&atilde;o">>.

get_base_url() -> 
	<<"/appconfigs">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>].

get_table_name() ->
	<<"appconfig">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	Name = appconfig_dataextractor:get_name(BodyPropList, ""),
    Description = appconfig_dataextractor:get_description(BodyPropList, ""),
    Site = appconfig_dataextractor:get_site(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
    if
      (OwnerId < 1) -> [messages_util:system_message(412)];
      ((Name == "") or (Description == "") or (Site == "")) -> [messages_util:system_message(100054)];
      true -> []
    end.

validate_to_update(BodyPropList, Id) -> 
	AppConfig = lists:nth(1, edit(Id)),
	Name = appconfig_dataextractor:get_name(BodyPropList, 
											proplists:get_value(<<"name">>, AppConfig, "")),
    Description = appconfig_dataextractor:get_description(BodyPropList,  
											proplists:get_value(<<"description">>, AppConfig, "")),
    Site = appconfig_dataextractor:get_site(BodyPropList, 
											proplists:get_value(<<"site">>, AppConfig, "")),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	ActiveNew = generic_dataextractor:get_bool(BodyPropList, "active", false),
	if
      ((Id < 1) or (OwnerId < 1)) -> [messages_util:system_message(412)];
	  ((Name == "") or (Description == "") or (Site == "")) -> [messages_util:system_message(100054)];
	  (ActiveNew == false) -> [];
      true -> 
		  case appconfig_dao:already_has_active(Id) of
			  true ->
				  [messages_util:system_message(100055)];
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
	Name = appconfig_dataextractor:get_name(BodyPropList, ""),
    Description = appconfig_dataextractor:get_description(BodyPropList, ""),
    Site = appconfig_dataextractor:get_site(BodyPropList, ""),
    UsePricingPolicy = generic_dataextractor:get_bool(BodyPropList, <<"usePricingPolicy">>, false),
    PricingPolicy = appconfig_dataextractor:get_pricing_policy(BodyPropList, ""),
    UsePrivacityPolicy = generic_dataextractor:get_bool(BodyPropList, <<"usePrivacityPolicy">>, false),
    PrivacityPolicy = appconfig_dataextractor:get_privacity_policy(BodyPropList, ""),
    UseUsetermsPolicy = generic_dataextractor:get_bool(BodyPropList, <<"useUsetermsPolicy">>, false),
    UsetermsPolicy = appconfig_dataextractor:get_useterms_policy(BodyPropList, ""),
    UseUsecontractPolicy = generic_dataextractor:get_bool(BodyPropList, <<"useUsecontractPolicy">>, false),
    UsecontractPolicy = appconfig_dataextractor:get_usecontract_policy(BodyPropList, ""),
    UseAuthorInfo = generic_dataextractor:get_bool(BodyPropList, <<"useAuthorInfo">>, false),
    AuthorInfo = appconfig_dataextractor:get_author_info(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	Created = appconfig_dao:create_object(Name, Description, Site, UsePricingPolicy, PricingPolicy, 
                                          UsePrivacityPolicy, PrivacityPolicy, UseUsetermsPolicy, 
                                          UsetermsPolicy, UseUsecontractPolicy, UsecontractPolicy,
			                              UseAuthorInfo, AuthorInfo, false, OwnerId),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(200)];
		_ ->
			[messages_util:system_message(100056)]
	end.

update(BodyPropList, Id, OwnerId, Token) -> 
	AppConfig = lists:nth(1, edit(Id)),
	Name = appconfig_dataextractor:get_name(BodyPropList, 
											proplists:get_value(<<"name">>, AppConfig, "")),
    Description = appconfig_dataextractor:get_description(BodyPropList,  
											proplists:get_value(<<"description">>, AppConfig, "")),
    Site = appconfig_dataextractor:get_site(BodyPropList, 
											proplists:get_value(<<"site">>, AppConfig, "")),
    UsePricingPolicy = generic_dataextractor:get_bool(BodyPropList, <<"usePricingPolicy">>,  
											   proplists:get_value(<<"usePricingPolicy">>, AppConfig, false)),
    PricingPolicy = appconfig_dataextractor:get_pricing_policy(BodyPropList, 
											  proplists:get_value(<<"pricingPolicy">>, AppConfig, "")),
    UsePrivacityPolicy = generic_dataextractor:get_bool(BodyPropList, <<"usePrivacityPolicy">>, 
												 proplists:get_value(<<"usePrivacityPolicy">>, AppConfig, false)),
    PrivacityPolicy = appconfig_dataextractor:get_privacity_policy(BodyPropList, 
												proplists:get_value(<<"privacityPolicy">>, AppConfig, "")),
    UseUsetermsPolicy = generic_dataextractor:get_bool(BodyPropList, <<"useUsetermsPolicy">>, 
												proplists:get_value(<<"useUsetermsPolicy">>, AppConfig, false)),
    UsetermsPolicy = appconfig_dataextractor:get_useterms_policy(BodyPropList, 
											   proplists:get_value(<<"usetermsPolicy">>, AppConfig, "")),
    UseUsecontractPolicy = generic_dataextractor:get_bool(BodyPropList, <<"useUsecontractPolicy">>, 
												   proplists:get_value(<<"useUsecontractPolicy">>, AppConfig, false)),
    UsecontractPolicy = appconfig_dataextractor:get_usecontract_policy(BodyPropList, 
												  proplists:get_value(<<"usecontractPolicy">>, AppConfig, "")),
    UseAuthorInfo = generic_dataextractor:get_bool(BodyPropList, <<"useAuthorInfo">>, 
												   proplists:get_value(<<"useAuthorInfo">>, AppConfig, false)),
    AuthorInfo = appconfig_dataextractor:get_author_info(BodyPropList, 
										   proplists:get_value(<<"authorInfo">>, AppConfig, "")),
	Active = generic_dataextractor:get_bool(BodyPropList, <<"active">>, 
									 proplists:get_value(<<"active">>, AppConfig, false)),
	ObjectOld = lists:nth(1, edit(Id)),
	Updated = appconfig_dao:update_object(Id, Name, Description, Site, UsePricingPolicy, 
										  PricingPolicy, UsePrivacityPolicy, PrivacityPolicy, 
										  UseUsetermsPolicy, UsetermsPolicy, UseUsecontractPolicy, 
										  UsecontractPolicy, UseAuthorInfo, AuthorInfo, Active),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, ObjectOld, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(100057)]
	end.

edit(Id) -> 
	appconfig_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) ->
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	appconfig_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) ->
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	appconfig_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case appconfig_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case appconfig_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case appconfig_dao:trully_delete_object(Id) of 
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
