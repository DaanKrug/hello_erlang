-module(userrecover_handler).

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
	<<"Recupera&ccedil;&atilde;o Acesso Usu&aacute;rio">>.

get_base_url() -> 
	<<"/userrecover">>.

get_max_access_times_on_minute() ->
	5.

get_categories() ->
	[].

get_table_name() ->
	<<"">>.

authorize_operation(_BodyPropList, _UrlParams, MethodToCall) ->
	MethodToCall == special_op1.

need_auth(_BodyPropList, _MethodToCall) ->
	false.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(_BodyPropList) -> 
	[messages_util:system_message(403)].

validate_to_update(_BodyPropList, _Id) -> 
	[messages_util:system_message(403)].

validate_to_delete(_Id) -> 
	[messages_util:system_message(403)].

validate_to_restore(_Id) -> 
	[messages_util:system_message(403)].

validate_to_trully_delete(_Id) -> 
	[messages_util:system_message(403)].

save(_BodyPropList, _OwnerId, _Token) -> 
	[messages_util:system_message(403)].

update(_BodyPropList, _Id, _OwnerId, _Token) -> 
	[messages_util:system_message(403)].

edit(_Id) -> 
	[messages_util:system_message(403)].

load_all(_BodyPropList, _AdditionalConditions, _Page, _Rows) -> 
	[messages_util:system_message(403)].

load_all_for_public(_BodyPropList, _AdditionalConditions, _Page, _Rows) -> 
	[messages_util:system_message(403)].

delete(_Id, _OwnerId, _Token) -> 
	[messages_util:system_message(403)].

restore(_Id, _OwnerId, _Token) -> 
	[messages_util:system_message(403)].

trully_delete(_Id, _OwnerId, _Token) -> 
	[messages_util:system_message(403)].

authenticate(_BodyPropList, _Host, _Token) ->
	false.

logoff(_BodyPropList, _Token) ->
	false.

special_op1(BodyPropList, OwnerId, Token) ->
	[recover(BodyPropList, OwnerId, Token)].

special_op2(_BodyPropList, _OwnerId, _Token) ->
	[messages_util:system_message(403)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
recover(BodyPropList, OwnerId, Token) ->
    Id = generic_dataextractor:get_id(BodyPropList),
	Email = user_dataextractor:get_email(BodyPropList, ""),
	if
		((Id /= -1) or (OwnerId == 0) or (Email == "") or (Token == "")) -> 
			messages_util:system_message(412);
		true -> 
			recover_user(Email, OwnerId)
	end.

recover_user(Email, OwnerId) ->
    Users = user_dao:load_for_recover(Email),
	case (length(Users) == 0) of
		true ->
			messages_util:system_message(205);
		_ ->
			User = lists:nth(1, Users),
			case user_dao:recover_user(Email) of 
				true ->
					sucessfully_recovered(User, OwnerId);
				_ ->
					messages_util:system_message(100011)
			end
	end.

sucessfully_recovered(User, OwnerId) ->
    UserOwner = get_user_owner(User, OwnerId),
	Id = proplists:get_value(<<"id">>, UserOwner),
	Name = proplists:get_value(<<"name">>, UserOwner),
	Email = proplists:get_value(<<"email">>, UserOwner),
	Encoded = json_util:encode(maplist_util:parse([UserOwner])),
	case applog_dao:create_object(Id, Name, Email, "Recuperou Senha", "Pessoa/Usu&aacute;rio", Encoded, "") of
		true ->
			messages_util:system_message(201);
		_ ->
			messages_util:system_message(100011)
	end.
    
get_user_owner(User, OwnerId) ->
	case (OwnerId < 1) of
		true ->
			User;
		_ ->
			user_dao:load_object_by_id(OwnerId)
	end.