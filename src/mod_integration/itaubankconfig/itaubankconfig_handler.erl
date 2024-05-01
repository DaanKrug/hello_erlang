-module(itaubankconfig_handler).

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
	<<"Integra&ccedil;&atilde;o Banco Ita&uacute;">>.

get_base_url() -> 
	<<"/itaubankconfigs">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>, <<"admin">>, <<"enroll">>].

get_table_name() ->
	<<"itaubankconfig">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	A1Itaukey = itaubankconfig_dataextractor:get_a1_itaukey(BodyPropList, ""),
    A2Shoplinecode = itaubankconfig_dataextractor:get_a2_shoplinecode(BodyPropList, ""),
    A3Shoplinekey = itaubankconfig_dataextractor:get_a3_shoplinekey(BodyPropList, ""),
    A4Shoplinesite = itaubankconfig_dataextractor:getA4_shoplinesite(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
    if
        (OwnerId < 1) -> [messages_util:system_message(412)];
	    ((A1Itaukey == "") or (A2Shoplinecode == "")) -> 
		    [messages_util:system_message(480, [object_class_name()])];
	    ((A3Shoplinekey == "") or (A4Shoplinesite == "")) ->
		    [messages_util:system_message(480, [object_class_name()])];
        true -> []
    end.

validate_to_update(BodyPropList, Id) -> 
	Itaubankconfig = lists:nth(1, edit(Id)),
	A1Itaukey = itaubankconfig_dataextractor:get_a1_itaukey(BodyPropList,
									           maplists:get_value(<<"a1_itaukey">>, Itaubankconfig, "")),
    A2Shoplinecode = itaubankconfig_dataextractor:get_a2_shoplinecode(BodyPropList, 
													maplists:get_value(<<"a2_shoplinecode">>, Itaubankconfig, "")),
    A3Shoplinekey = itaubankconfig_dataextractor:get_a3_shoplinekey(BodyPropList, 
												   maplists:get_value(<<"a3_shoplinekey">>, Itaubankconfig, "")),
    A4Shoplinesite = itaubankconfig_dataextractor:get_a4_shoplinesite(BodyPropList, 
													maplists:get_value(<<"a4_shoplinesite">>, Itaubankconfig, "")),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	if
        ((Id < 1) or (OwnerId < 1)) -> [messages_util:system_message(412)];
		((A1Itaukey == "") or (A2Shoplinecode == "")) -> 
		    [messages_util:system_message(412)];
	    ((A3Shoplinekey == "") or (A4Shoplinesite == "")) ->
		    [messages_util:system_message(412)];
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
	A1Itaukey = itaubankconfig_dataextractor:get_a1_itaukey(BodyPropList, ""),
    A2Shoplinecode = itaubankconfig_dataextractor:get_a2_shoplinecode(BodyPropList, ""),
    A3Shoplinekey = itaubankconfig_dataextractor:get_a3_shoplinekey(BodyPropList, ""),
    A4Shoplinesite = itaubankconfig_dataextractor:get_a4_shoplinesite(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	Created = itaubankconfig_dao:create_object(A1Itaukey, A2Shoplinecode, A3Shoplinekey, A4Shoplinesite, OwnerId),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(200)];
		_ ->
			[messages_util:system_message(100232)]
	end.

update(BodyPropList, Id, OwnerId, Token) -> 
	Itaubankconfig = lists:nth(1, edit(Id)),
	A1Itaukey = itaubankconfig_dataextractor:get_a1_itaukey(BodyPropList,
									           maplists:get_value(<<"a1_itaukey">>, Itaubankconfig, "")),
    A2Shoplinecode = itaubankconfig_dataextractor:get_a2_shoplinecode(BodyPropList, 
													maplists:get_value(<<"a2_shoplinecode">>, Itaubankconfig, "")),
    A3Shoplinekey = itaubankconfig_dataextractor:get_a3_shoplinekey(BodyPropList, 
												   maplists:get_value(<<"a3_shoplinekey">>, Itaubankconfig, "")),
    A4Shoplinesite = itaubankconfig_dataextractor:get_a4_shoplinesite(BodyPropList, 
													maplists:get_value(<<"a4_shoplinesite">>, Itaubankconfig, "")),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	Updated = itaubankconfig_dao:update_object(Id, A1Itaukey, A2Shoplinecode, A3Shoplinekey, A4Shoplinesite),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, Itaubankconfig, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(100232)]
	end.

edit(Id) -> 
	itaubankconfig_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	itaubankconfig_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	itaubankconfig_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case itaubankconfig_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case itaubankconfig_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case itaubankconfig_dao:trully_delete_object(Id) of 
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
	[generate_billet(BodyPropList, OwnerId, Token)].

special_op2(BodyPropList, OwnerId, Token) ->
	[generate_search_data(BodyPropList, OwnerId, Token)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_billet(BodyPropList, OwnerId, Token) ->
    A1Itaukey = itaubankconfig_dataextractor:get_a1_itaukey(BodyPropList, ""),
    A2Shoplinecode = itaubankconfig_dataextractor:get_a2_shoplinecode(BodyPropList, ""),
    A3Shoplinekey = itaubankconfig_dataextractor:get_a3_shoplinekey(BodyPropList, ""),
	A4Shoplinesite = itaubankconfig_dataextractor:get_a4_shoplinesite(BodyPropList, ""),
    OrderNumber = itaubankconfig_dataextractor:get_order_number(BodyPropList, ""),
	OrderValue = itaubankconfig_dataextractor:get_order_value(BodyPropList, ""),
	Observation = itaubankconfig_dataextractor:get_observation(BodyPropList, ""),
    OwnerName = itaubankconfig_dataextractor:get_owner_name(BodyPropList, ""),
    CpfInscription = itaubankconfig_dataextractor:get_cpf_inscription(BodyPropList, ""),
    CpfNumber = itaubankconfig_dataextractor:get_cpf_number(BodyPropList, ""),
    OwnerAddress = itaubankconfig_dataextractor:get_owner_address(BodyPropList, ""),
    OwnerNeighborhood = itaubankconfig_dataextractor:get_owner_neighborhood(BodyPropList, ""),
    OwnerCep = itaubankconfig_dataextractor:get_owner_cep(BodyPropList, ""),
    OwnerCity = itaubankconfig_dataextractor:get_owner_city(BodyPropList, ""),
    OwnerState = itaubankconfig_dataextractor:get_owner_state(BodyPropList, ""),
    ExpiresDate = itaubankconfig_dataextractor:get_expires_date(BodyPropList, ""),
    UrlRetorno = itaubankconfig_dataextractor:get_url_retorno(BodyPropList, ""),
    AdditionalText1 = itaubankconfig_dataextractor:get_additional_text1(BodyPropList, ""),
    AdditionalText2 = itaubankconfig_dataextractor:get_additional_text2(BodyPropList, ""),
    AdditionalText3 = itaubankconfig_dataextractor:get_additional_text3(BodyPropList, A4Shoplinesite),
    if
		(OwnerId < 1) -> 
			messages_util:system_message(412);
		((Token == "") or (A1Itaukey == "") or (OrderNumber == "")) -> 
			messages_util:system_message(412);
		((A2Shoplinecode == "") or (A3Shoplinekey == "")) ->
			messages_util:system_message(412);
		((OrderValue == "") or (OwnerName == "")) ->
			messages_util:system_message(412);
		((CpfInscription == "") or (CpfNumber == "") or (OwnerAddress == "")) ->
			messages_util:system_message(412);
		((OwnerNeighborhood == "") or (OwnerCep == "") or (OwnerCity == "")) ->
			messages_util:system_message(412);
		((OwnerState == "") or (ExpiresDate == "") or (UrlRetorno == "")) ->
			messages_util:system_message(412);
		true -> 
			generate_billet2(A1Itaukey, A2Shoplinecode, OrderNumber, OrderValue, Observation, A3Shoplinekey, 
							 OwnerName, CpfInscription, CpfNumber, OwnerAddress, OwnerNeighborhood, OwnerCep, 
							 OwnerCity, OwnerState, ExpiresDate, UrlRetorno, AdditionalText1, AdditionalText2, 
                             AdditionalText3)
	end.

generate_billet2(A1Itaukey, A2Shoplinecode, OrderNumber, OrderValue, Observation, A3Shoplinekey,
				 OwnerName, CpfInscription, CpfNumber, OwnerAddress, OwnerNeighborhood, OwnerCep,
				 OwnerCity, OwnerState, ExpiresDate, UrlRetorno, AdditionalText1, AdditionalText2,
				 AdditionalText3) ->
	BilletData = itaubankconfig_util:get_bank_billet_data(A1Itaukey, A2Shoplinecode, OrderNumber, 
														  OrderValue, Observation, A3Shoplinekey,
				                                          OwnerName, CpfInscription, CpfNumber, 
														  OwnerAddress, OwnerNeighborhood, OwnerCep,
				                                          OwnerCity, OwnerState, ExpiresDate, UrlRetorno, 
														  AdditionalText1, AdditionalText2, AdditionalText3),
	HasError = string:find(<<"Erro:">>, BilletData),
	case HasError of
		nomatch ->
			return_util:get_report(BilletData);
		_ ->
			messages_util:system_message(400)
	end.

generate_search_data(BodyPropList, OwnerId, Token) ->
	A1Itaukey = itaubankconfig_dataextractor:get_a1_itaukey(BodyPropList, ""),
    A2Shoplinecode = itaubankconfig_dataextractor:get_a2_shoplinecode(BodyPropList, ""),
    A3Shoplinekey = itaubankconfig_dataextractor:get_a3_shoplinekey(BodyPropList, ""),
    OrderNumber = itaubankconfig_dataextractor:get_order_number(BodyPropList, ""),
    Format = itaubankconfig_dataextractor:get_format(BodyPropList, ""),
	if
		(OwnerId < 1) -> 
			messages_util:system_message(412);
		((Token == "") or (A1Itaukey == "") or (OrderNumber == "")) -> 
			messages_util:system_message(412);
		((A2Shoplinecode == "") or (A3Shoplinekey == "")) ->
			messages_util:system_message(412);
		true -> 
			generate_search_data2(A1Itaukey, A2Shoplinecode, OrderNumber, Format, A3Shoplinekey)
	end.

generate_search_data2(A1Itaukey, A2Shoplinecode, OrderNumber, Format, A3Shoplinekey) ->
	SearchData = itaubankconfig_util:get_search_data(A1Itaukey, A2Shoplinecode, 
													 OrderNumber, Format, A3Shoplinekey),
	HasError = string:find(<<"Erro:">>, SearchData),
	case HasError of
		nomatch ->
			return_util:get_report(SearchData);
		_ ->
			messages_util:system_message(400)
	end.











