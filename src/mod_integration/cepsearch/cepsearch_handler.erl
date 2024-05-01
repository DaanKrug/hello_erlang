-module(cepsearch_handler).

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
	"Busca CEP".

get_base_url() -> 
	"/cepsearch".

get_max_access_times_on_minute() ->
	5.

get_categories() ->
	[<<"admin_master">>, <<"admin">>].

get_table_name() ->
	<<"">>.

authorize_operation(_BodyPropList, _UrlParams, MethodToCall) ->
	MethodToCall == special_op1.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

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

special_op1(BodyPropList, _OwnerId, _Token) ->
	search_cep(BodyPropList).

special_op2(_BodyPropList, _OwnerId, _Token) ->
	[messages_util:system_message(403)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search_cep(BodyPropList) ->
    % https://brasilapi.com.br/api/cep/v1/#{cep} TODO
	Cep = generic_dataextractor:get_cep(BodyPropList),
	Url = lists:concat(["viacep.com.br/ws/", Cep, "/json/"]),
    http_request_util:make_get_request(Url, false).











