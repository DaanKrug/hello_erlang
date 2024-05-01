-module(wallettransaction_handler).

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
	<<"Wallet Transaction">>.

get_base_url() -> 
	<<"/wallettransactions">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>].

get_table_name() ->
	<<"wallettransaction">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(BodyPropList, MethodToCall) ->
	case (MethodToCall == save) of 
		true ->
			OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
			Id = generic_dataextractor:get_id(BodyPropList),
			if
				((OwnerId == 0) and (Id == -1)) -> false;
				true -> true
			end;
		_ ->
			true
	end.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	Id = generic_dataextractor:get_id(BodyPropList),
	IdentifierSender = wallettransaction_dataextractor:get_identifier_sender(BodyPropList, ""),
	IdentifierReceiver = wallettransaction_dataextractor:get_identifier_receiver(BodyPropList, ""),
	AmountCoins = wallettransaction_dataextractor:get_amount_coins(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	%io:format("~p",[[Id,IdentifierSender,  IdentifierReceiver, AmountCoins, OwnerId]]),
    if
		((OwnerId < 1) and (Id /= -1)) -> [messages_util:system_message(412)];
        ((AmountCoins == 0)  or (AmountCoins < 0)) -> [messages_util:system_message(412)];
		(IdentifierSender == "") -> [messages_util:system_message(412)];
		(IdentifierReceiver == "") -> [messages_util:system_message(412)];
		(IdentifierSender == IdentifierReceiver) -> [messages_util:system_message(412)];
        true -> validate_sender_and_receiver(IdentifierSender, IdentifierReceiver, AmountCoins)
    end.

validate_to_update(BodyPropList, Id) -> 
	Id2 = number_util:to_integer(Id),
	Status = wallettransaction_dataextractor:get_status(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	if
        ((Id2 < 1) or (OwnerId < 1) or (Status == "")) -> [messages_util:system_message(412)];
	    true -> []
    end.

validate_to_delete(Id) -> 
	Id2 = number_util:to_integer(Id),
	if
        (Id2 < 1) -> [messages_util:system_message(412)];
	    true -> 
			WalletTransaction = lists:nth(1, edit(Id)),
			Status = proplists:get_value(<<"status">>, WalletTransaction, ""),
			case (Status == <<"pending">>) of
				true ->
					[];
				_ ->
					[messages_util:system_message(412)]
			end
    end.

validate_to_restore(Id) -> 
	Id2 = number_util:to_integer(Id),
	if
        (Id2 < 1) -> [messages_util:system_message(412)];
	    true -> 
			WalletTransaction = lists:nth(1, edit(Id)),
			IdentifierSender = proplists:get_value(<<"identifierSender">>, WalletTransaction, ""),
			IdentifierReceiver = proplists:get_value(<<"identifierReceiver">>, WalletTransaction, ""),
			AmountCoins = proplists:get_value(<<"amountCoins">>, WalletTransaction, ""),
			validate_sender_and_receiver(IdentifierSender, IdentifierReceiver, AmountCoins)
    end.

validate_to_trully_delete(Id) -> 
	Id2 = number_util:to_integer(Id),
	if
        (Id2 < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

save(BodyPropList, OwnerId, Token) -> 
	IdentifierSender = wallettransaction_dataextractor:get_identifier_sender(BodyPropList, ""),
	IdentifierReceiver = wallettransaction_dataextractor:get_identifier_receiver(BodyPropList, ""),
	Description = wallettransaction_dataextractor:get_description(BodyPropList, ""),
	AmountCoins = wallettransaction_dataextractor:get_amount_coins(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	OwnerId2 = case ((undefined == OwnerId) or (OwnerId < 1)) of
				   true ->
					   AdminIds = user_dao:load_masteradmin_ids(),
					   PropList0 = lists:nth(1, AdminIds),
					   proplists:get_value(<<"id">>, PropList0, 0);
				   _ ->
					   OwnerId
			   end,
	Created = wallettransaction_dao:create_object(IdentifierSender, IdentifierReceiver, Description, 
												  AmountCoins, OwnerId2),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			case (block_dao:append_transaction_to_blockchain(Object)) of
				true ->
					applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
					[messages_util:system_message(200)];
				_ ->
					wallettransaction_dao:delete_object(Id),
					[messages_util:system_message(100285)]
			end;
		_ ->
			[messages_util:system_message(100285)]
	end.

update(BodyPropList, Id, OwnerId, Token) -> 
	WalletTransaction = lists:nth(1, edit(Id)),
	StatusOld = proplists:get_value(<<"status">>, WalletTransaction, ""),
	Status = wallettransaction_dataextractor:get_status(BodyPropList, StatusOld),
	Updated = wallettransaction_dao:update_object(Id, Status),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, WalletTransaction, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(100286)]
	end.

edit(Id) -> 
	wallettransaction_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	wallettransaction_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	Identifier = wallet_dataextractor:get_identifier(BodyPropList, ""),
	case validate_wallet_access(Identifier, BodyPropList) of
		true ->
			Identifier2 = binary_to_list(Identifier),
			ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
			Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
			Conditions2 = lists:concat([
										Conditions, " ", AdditionalConditions,
										" and (identifierSender = '", Identifier2, 
										"' or identifierReceiver = '", Identifier2, "') "
									   ]),
			FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
			wallettransaction_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted);
		_ ->
			[]
	end.

delete(Id, OwnerId, Token) -> 
	case wallettransaction_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case wallettransaction_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case wallettransaction_dao:trully_delete_object(Id) of 
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

validate_sender_and_receiver(IdentifierSender, IdentifierReceiver, AmountCoins) ->
	Senders = wallet_dao:load_by_identifier(IdentifierSender),
	Receivers = wallet_dao:load_by_identifier(IdentifierReceiver),
	case ((length(Senders) == 0) or (length(Receivers) == 0)) of 
		true ->
			[messages_util:system_message(412)];
		_ ->
			Sender = lists:nth(1, Senders),
			Active = proplists:get_value(<<"active">>, Sender, 0),
			TotalCoins = proplists:get_value(<<"totalCoins">>, Sender, 0),
			case (Active == 0) or (TotalCoins < AmountCoins) of
				true ->
					[messages_util:system_message(412)];
				_ ->
					[]
			end
	end.

validate_wallet_access(Identifier, BodyPropList) ->
	case (Identifier == "") of
		true ->
			false;
		_ ->
			Wallets = wallet_dao:load_by_identifier(Identifier),
			case (length(Wallets) == 0) of
				true ->
					false;
				_ ->
					Wallet = lists:nth(1, Wallets),
					HashedPassword = proplists:get_value(<<"password">>, Wallet, ""),
					Password = wallet_dataextractor:get_password(BodyPropList),
					hash_util:password_match(HashedPassword, Password)
			end
	end.