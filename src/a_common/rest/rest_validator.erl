-module(rest_validator).

-export(
   		[
		 validate_access_to_resource/4,
		 validate_auth/7,
		 validate_ownership/5
		]
	   ).

validate_access_to_resource(Host, Token, Resource, MaxTimes) ->
	Validated = sessioncontrol_validator:validate_access_by_history_access(Host, Token, Resource, MaxTimes),
	case Validated of
		true ->
			sessioncontrol_validator:store_history_access(Host, Token, Resource);
		_ ->
			false
	end.

validate_auth(Host, Token, Module, MethodToCall, BodyPropList, UrlParams, OwnerId) ->
	NeedAuth = Module:need_auth(BodyPropList, MethodToCall),
	case NeedAuth of
		external ->
			Module:authenticate(BodyPropList, Host, Token);
		false ->
			case ((undefined == OwnerId) or (OwnerId < 1)) of 
				true ->
					Module:authorize_operation(BodyPropList, UrlParams, MethodToCall);
				_ ->
					authorize_operation(Token, Module, MethodToCall, BodyPropList, UrlParams, OwnerId)
			end;
		_ ->
			Authenticated = sessioncontrol_validator:is_authenticated(Host, Token, OwnerId),
			case (Authenticated == true) of
				true ->
					authorize_operation(Token, Module, MethodToCall, BodyPropList, UrlParams, OwnerId);
				_ -> 
					false
			end
	end.

validate_ownership(MethodToCall, OwnerId, Token, Module, UrlParams) ->
	SkipValidation = Module:skip_validate_ownership(MethodToCall, OwnerId, Token, UrlParams),
	case SkipValidation of
		true ->
			true;
		_ ->
			case lists:member(MethodToCall, [update, delete, restore, trully_drop]) of
				true ->
					sessioncontrol_validator:validate_ownership(OwnerId, Token, Module, lists:nth(1, UrlParams));
				_ ->
					true
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_permission(TableName, MethodToCall) ->
	IsMember = lists:member(MethodToCall, [save, update, delete, restore, trully_drop]),
	case (IsMember == true) of
		true ->
			lists:concat([
						  string_util:to_list(string_util:to_string(TableName)), 
						  "_write"
						 ]);
		_ ->
			TableName
	end.

authorize_operation(Token, Module, MethodToCall, BodyPropList, UrlParams, OwnerId) ->
	Authorized = Module:authorize_operation(BodyPropList, UrlParams, MethodToCall),
	case (Authorized == true) of
		true ->
			Permission = get_permission(Module:get_table_name(), MethodToCall),
			sessioncontrol_validator:validate_access(OwnerId, Token, Module:get_categories(), Permission);
		_ ->
			false
	end.

