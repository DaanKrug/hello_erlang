-module(generic_dataextractor).

-export(
   		[
		 get_cep/1,
		 get_language/1,
		 get_key/1, 
		 get_rkey/1, 
		 get_bool/3, 
		 get_position/2, 
		 get_start/2, 
		 get_finish/2,
		 get_id/1,
		 get_owner_id/1,
		 get_token/1,
		 get_random_key/1,
		 get_ip/1,
		 get_conditions/1
		]
	   ).

get_cep(BodyPropList) ->
	Cep = proplists:get_value(<<"cep">>, BodyPropList, ""),
	Cep2 = string:lowercase(string_util:trim(Cep)),
    sanitizer_util:sanitize_all(Cep2, false, 9, <<"-,1,2,3,4,5,6,7,8,9">>).

get_language(BodyPropList) ->
	Key = proplists:get_value(<<"language">>, BodyPropList, "pt_br"),
	string_util:trim(Key).
	
get_key(BodyPropList) ->
	Key = proplists:get_value(<<"key">>, BodyPropList, ""),
	Key2 = string_util:trim(Key),
	Key3 = string:uppercase(Key2),
    sanitizer_util:sanitize_all(Key3, false, 0, <<"hex">>).

get_rkey(BodyPropList) ->
	Rkey = proplists:get_value(<<"rkey">>, BodyPropList, ""),
	Rkey2 = string_util:trim(Rkey),
    sanitizer_util:sanitize_all(Rkey2, false, 0, <<"A-z0-9">>).

get_bool(BodyPropList, Key, DefaultValue) ->
	Bool = proplists:get_value(Key, BodyPropList, DefaultValue),
	(Bool == true) or (Bool == 1) or (Bool == <<"true">>) or (Bool == <<"1">>).
  
get_position(BodyPropList, DefaultValue) ->
	Position = proplists:get_value(<<"position">>, BodyPropList, DefaultValue),
	Position2 = sanitizer_util:sanitize_all(Position, true, 3, undefined),
	%Position3 = list_to_integer(string_util:to_string(Position2)),
	Position3 = number_util:to_integer(Position2),
    number_util:coalesce_interval(Position3, 1, 999).

get_start(BodyPropList, DefaultValue) ->
	Start = proplists:get_value(<<"start">>, BodyPropList, DefaultValue),
	sanitizer_util:sanitize_all(Start, false, 19, <<"DATE_SQL">>).

get_finish(BodyPropList, DefaultValue) ->
	Finish = proplists:get_value(<<"finish">>, BodyPropList, DefaultValue),
	sanitizer_util:sanitize_all(Finish, false, 19, <<"DATE_SQL">>).

get_id(BodyPropList) ->
	Id = proplists:get_value(<<"id">>, BodyPropList, 0),
	Id2 = sanitizer_util:sanitize_all(Id, true, 20, <<"0-9">>),
	number_util:to_integer(Id2).

get_owner_id(BodyPropList) ->
	OwnerId = proplists:get_value(<<"ownerId">>, BodyPropList, 0),
	OwnerId2 = sanitizer_util:sanitize_all(OwnerId, true, 20, <<"0-9">>),
	number_util:to_integer(OwnerId2).

get_token(BodyPropList) ->
	Token = proplists:get_value(<<"_token">>, BodyPropList, ""),
	sanitizer_util:sanitize_all(Token, false, 250, <<"A-z0-9">>).

get_random_key(BodyPropList) ->
	RandomKey = proplists:get_value(<<"randomKey">>, BodyPropList, ""),
    sanitizer_util:sanitize_all(RandomKey, false, 50, <<"A-z0-9">>).

get_ip(BodyPropList) ->
    Ip = proplists:get_value(<<"ip">>, BodyPropList, ""),
    sanitizer_util:sanitize_all(Ip, false, 30, <<"A-z0-9">>).

get_conditions(BodyPropList) ->
	Conditions = proplists:get_value(<<"conditions">>, BodyPropList, ""),
	case (datatransform_util:is_undefined(Conditions)) of
		true ->
			["", false];
		_ -> 
			Conditions2 = sanitizer_util:sanitize(string_util:trim(Conditions)),
			AuditingExclusions = string:find(Conditions2, <<"0x{auditingExclusions}">>),
			case AuditingExclusions of
				nomatch ->
					[Conditions2, false];
				_ ->
					Conditions3 = string:replace(Conditions2, <<"0x{auditingExclusions}">>, "", all),
					[Conditions3, true]
			end
	end.
	