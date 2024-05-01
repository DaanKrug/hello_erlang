-module(externalsimplemail_handler).

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
	"Email".

get_base_url() -> 
	"/externalsimplemails".

get_max_access_times_on_minute() ->
	5.

get_categories() ->
	[<<"external">>].

get_table_name() ->
	<<"simplemail">>.

authorize_operation(_BodyPropList, _UrlParams, MethodToCall) ->
	MethodToCall == save.

need_auth(_BodyPropList, MethodToCall) ->
	case (MethodToCall == save) of
		true ->
			external;
		_ ->
			true
	end.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	simplemail_handler:validate_to_save(BodyPropList).

validate_to_update(_BodyPropList, _Id) -> 
	[messages_util:system_message(403)].

validate_to_delete(_Id) -> 
	[messages_util:system_message(403)].

validate_to_restore(_Id) -> 
	[messages_util:system_message(403)].

validate_to_trully_delete(_Id) -> 
	[messages_util:system_message(403)].

save(BodyPropList, OwnerId, Token) -> 
	simplemail_handler:save(BodyPropList, OwnerId, Token).

update(_BodyPropList, _Id, _OwnerId, _Token) -> 
	messages_util:system_message(100017).

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

authenticate(BodyPropList, Host, Token) ->
	auth_handler:login_auth_only(BodyPropList, Host, Token).

logoff(BodyPropList, Token) ->
	auth_handler:logoff_auth_only(BodyPropList, Token).

special_op1(_BodyPropList, _OwnerId, _Token) ->
	[messages_util:system_message(403)].

special_op2(_BodyPropList, _OwnerId, _Token) ->
	[messages_util:system_message(403)].




