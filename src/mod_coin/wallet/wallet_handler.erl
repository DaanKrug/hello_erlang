-module(wallet_handler).

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
	<<"Wallet">>.

get_base_url() -> 
	<<"/wallets">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>].

get_table_name() ->
	<<"wallet">>.

authorize_operation(BodyPropList, _UrlParams, MethodToCall) ->
	case ((MethodToCall == update) or (MethodToCall == save)) of 
		true ->
			OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
			Id = generic_dataextractor:get_id(BodyPropList),
			if
				((OwnerId == 0) and (Id == -1)) -> true;
				((OwnerId == -1) and (Id == -2)) -> true;
				((OwnerId < 1) or (Id < 1)) -> false;
				true -> true
			end;
		_ ->
			true
	end.

need_auth(BodyPropList, MethodToCall) ->
	case ((MethodToCall == update) or (MethodToCall == save)) of 
		true ->
			OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
			Id = generic_dataextractor:get_id(BodyPropList),
			if
				((OwnerId == 0) and (Id == -1)) -> false;
				((OwnerId == -1) and (Id == -2)) -> false;
				true -> true
			end;
		_ ->
			true
	end.

skip_validate_ownership(MethodToCall, OwnerId, Token, UrlParams) ->
	case ((MethodToCall == update) and (Token /= "")) of 
		true ->
			Id = number_util:to_integer(lists:nth(1, UrlParams)),
			if
				((OwnerId == 0) and (Id == -1)) -> true;
				((OwnerId == -1) and (Id == -2)) -> true;
				true -> false
			end;
		_ ->
			false
	end.

validate_to_save(BodyPropList) -> 
	Id = generic_dataextractor:get_id(BodyPropList),
	Identifier = wallet_dataextractor:get_identifier(BodyPropList, ""), 
	Password = wallet_dataextractor:get_password(BodyPropList),
	RecoverInfo = wallet_dataextractor:get_recoverinfo(BodyPropList, ""), 
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
    if
        ((OwnerId < 1)  and (Id /= -1)) -> [messages_util:system_message(412)];
		((Identifier == "") or (Password == "") or (RecoverInfo == "")) -> [messages_util:system_message(412)];
        true -> []
    end.

validate_to_update(BodyPropList, Id) ->  
	Identifier = wallet_dataextractor:get_identifier(BodyPropList, ""), 
	RecoverInfo = wallet_dataextractor:get_recoverinfo(BodyPropList, ""), 
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	Id2 = number_util:to_integer(Id),
	if
		((OwnerId == 0) and (Id2 == -1) and (Identifier /= "")) -> [];
		((OwnerId == -1) and (Id2 == -2) and (RecoverInfo /= "")) -> [];
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

save(BodyPropList, OwnerId, Token) -> 
	Identifier = wallet_dataextractor:get_identifier(BodyPropList, ""), 
	Password0 = wallet_dataextractor:get_password(BodyPropList),
	Password = hash_util:hash_password(Password0),
	RecoverInfo = wallet_dataextractor:get_recoverinfo(BodyPropList, ""), 
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	OwnerId2 = case ((undefined == OwnerId) or (OwnerId < 1)) of
				   true ->
					   AdminIds = user_dao:load_masteradmin_ids(),
					   PropList0 = lists:nth(1, AdminIds),
					   proplists:get_value(<<"id">>, PropList0, 0);
				   _ ->
					   OwnerId
			   end,
	Created = wallet_dao:create_object(Identifier, Password, RecoverInfo, OwnerId2),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(200)];
		_ ->
			[messages_util:system_message(100279)]
	end.

update(BodyPropList, Id, OwnerId, Token) -> 
	Wallet = load_wallet(Id, BodyPropList),
	case (undefined == Wallet) of
		true ->
			update_error_message(Id);
		_ ->
			PasswordNew = get_new_password(Id, BodyPropList),
			case (PasswordNew == "") of
				true ->
					OwnerId2 = proplists:get_value(<<"ownerId">>, Wallet, 0),
					OwnerId3 = number_util:to_integer(OwnerId),
					case ((OwnerId < 1) or (OwnerId2 /= OwnerId3)) of
						true ->
							update_error_message(Id);
						_ ->
							Id2 = proplists:get_value(<<"id">>, Wallet, 0),
							Password = proplists:get_value(<<"password">>, Wallet, ""),
							Active = get_active(Id, BodyPropList),
							make_update(Wallet, Id, Id2, Password, Active, OwnerId, Token)
					end;
				_ ->
					Active = get_active(Id, BodyPropList),
					Id2 = proplists:get_value(<<"id">>, Wallet, 0),
					make_update(Wallet, Id, Id2, PasswordNew, Active, OwnerId, Token)
			end
	end.

edit(Id) -> 
	wallet_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	wallet_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, _AdditionalConditions, _Page, _Rows) -> 
	Wallet = get_wallet_by_identifier(BodyPropList),
	case (undefined == Wallet) of
		true ->
			[];
		_ ->
			case (proplists:get_value(<<"active">>, Wallet, 0) == 1) of
				true ->
					[Wallet];
				_ -> 
					[]
			end
	end.
	
delete(Id, OwnerId, Token) -> 
	case wallet_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case wallet_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case wallet_dao:trully_delete_object(Id) of 
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
make_update(Wallet, Id, Id2, PasswordNew, Active, OwnerId, Token) ->
	Updated = wallet_dao:update_object(Id2, PasswordNew, Active),
	case Updated of 
		true ->
			reactivate_wallet(Id2, proplists:get_value(<<"active">>, Wallet, 0)),
			ObjectNew = lists:nth(1, edit(Id2)),
			applog_util:successfully_updated(OwnerId, Wallet, ObjectNew, object_class_name(), Token),
			update_success_message(Id);
		_ ->
			update_error_message(Id)
	end.

get_active(Id, BodyPropList) ->
	Id2 = number_util:to_integer(Id),
	case Id2 of
		-2 ->
			true;
		_ ->
			generic_dataextractor:get_bool(BodyPropList, <<"active">>, true)
	end.
	
get_new_password(Id, BodyPropList) ->
	Id2 = number_util:to_integer(Id),
	case Id2 of
		-1 ->
			hash_util:hash_password(wallet_dataextractor:get_new_password(BodyPropList));
		_ ->
			hash_util:hash_password(wallet_dataextractor:get_password(BodyPropList))
	end.
			
load_wallet(Id, BodyPropList) ->
	Id2 = number_util:to_integer(Id),
	case Id2 of
		-1 ->
			get_wallet_by_identifier(BodyPropList);
		-2 ->
			get_wallet_by_recoverinfo(BodyPropList);
		_ ->
			lists:nth(1, edit(Id))
	end.

update_success_message(Id) ->
	Id2 = number_util:to_integer(Id),
	case Id2 of
		-1 ->
			[messages_util:system_message(100283)];
		-2 ->
			[messages_util:system_message(100284)];
		_ ->
			[messages_util:system_message(201)]
	end.

update_error_message(Id) ->
	Id2 = number_util:to_integer(Id),
	case Id2 of
		-1 ->
			[messages_util:system_message(100281)];
		-2 ->
			[messages_util:system_message(100282)];
		_ ->
			[messages_util:system_message(100279)]
	end.

get_wallet_by_identifier(BodyPropList) ->
	Identifier = wallet_dataextractor:get_identifier(BodyPropList, ""),
	case (Identifier == "") of
		true ->
			undefined;
		_ ->
			Wallets = wallet_dao:load_by_identifier(Identifier),
			get_verified_password_wallet(Wallets, BodyPropList)
	end.

get_wallet_by_recoverinfo(BodyPropList) ->
	Identifier = wallet_dataextractor:get_identifier(BodyPropList, ""),
	RecoverInfo = wallet_dataextractor:get_recoverinfo(BodyPropList, ""),
	case ((Identifier == "") or (RecoverInfo == "")) of
		true ->
			undefined;
		_ ->
			Wallets = wallet_dao:load_by_recoverinfo(Identifier, RecoverInfo),
			case (length(Wallets) == 0) of
				true ->
					undefined;
				_ ->
					lists:nth(1, Wallets)
			end
	end.

get_verified_password_wallet(Wallets, BodyPropList) ->
	case (length(Wallets) == 0) of
		true ->
			undefined;
		_ ->
			Wallet = lists:nth(1, Wallets),
			LoginFails = proplists:get_value(<<"loginFails">>, Wallet, 0),
			case (LoginFails > 2) of
				true ->
					undefined;
				_ ->
					HashedPassword = proplists:get_value(<<"password">>, Wallet, ""),
					Password = wallet_dataextractor:get_password(BodyPropList),
					case (hash_util:password_match(HashedPassword, Password)) of
						true ->
							Wallet2 = proplists:delete(<<"password">>, Wallet),
							[{<<"password">>, Password} | Wallet2];
						_ -> 
							Id = proplists:get_value(<<"id">>, Wallet, 0),
							wallet_dao:set_login_fail(Id, LoginFails + 1),
							undefined
					end
			end
	end.

reactivate_wallet(Id, Active) ->
	case (Active == 1) of
		true ->
			true;
		_ ->
			wallet_dao:set_login_fail(Id, 0)
	end.









