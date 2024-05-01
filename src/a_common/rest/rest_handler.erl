-module(rest_handler).

-export([handle_request/3, send_result/6]).

handle_request(Req0, State, Module) ->
	Resource = Module:get_base_url(),
	MaxTimes = Module:get_max_access_times_on_minute(),
	Req = request_util:enable_rest_and_cors(Req0),
	Host = string_util:trim(request_util:get_host(Req)),
	BodyPropList = request_util:get_body(Req),
	Token = generic_dataextractor:get_token(BodyPropList),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	case ((Host == "") or (Token == "")) of
		true ->
			send_result(400, [rest_util:bad_request()], Req, State, undefined, true);
		_ ->
			ValidatedAccess = rest_validator:validate_access_to_resource(Host, Token, Resource, MaxTimes),
			case ValidatedAccess of
				false ->
					send_result(429, [rest_util:too_many_requests()], Req, State, undefined, true);
				_ ->
					handle_request_auth(Req, State, Module, Resource, Host, BodyPropList, Token, OwnerId)	
			end
	end.

send_result(Code, PropList, Req, State, GetSntp, AsSingleObject) ->
	PropList2 = sntp_service:append_sntp(PropList, GetSntp),
	Encoded = case AsSingleObject of
				  true ->
					  PropListObject = lists:nth(1, PropList2),
				  	  Map = maps:from_list(PropListObject),
					  json_util:encode_map(Map);
				  _ ->
					  MapList = maplist_util:parse(PropList2),
					  json_util:encode(MapList)
			  end,
	ContentType = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
	Req1 = cowboy_req:reply(Code, ContentType, Encoded, Req),
	%io:format(" ~p ", [Encoded]),
	{ok, Req1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_request_auth(Req, State, Module, BaseUrl, Host, BodyPropList, Token, OwnerId) ->
	Method = cowboy_req:method(Req),
	Path = cowboy_req:path(Req),
	MethodToCallAndUrlParams = rest_service:parse_req_get_method(BaseUrl, Method, Path),
	case MethodToCallAndUrlParams of
		undefined ->
			send_result(500, [rest_util:method_not_found()], Req, State, undefined, true);
		_ ->
			[MethodToCall | UrlParams] = MethodToCallAndUrlParams,
			handle_requested_method(MethodToCall, UrlParams, BodyPropList, Req, State, 
									Module, Host, Token, OwnerId)
	end.

handle_requested_method(MethodToCall, UrlParams, BodyPropList, Req, State, 
						Module, Host, Token, OwnerId) ->
	AuthorizedOperation = rest_validator:validate_auth(Host, Token, Module, MethodToCall, 
													   BodyPropList, UrlParams, OwnerId),
	case AuthorizedOperation of
		false ->
			handle_req_method_op_not_ok(MethodToCall, Req, State, BodyPropList, 
										Module, UrlParams, OwnerId, Token);
		_ ->
			handle_req_method_op_ok(MethodToCall, Req, State, BodyPropList, 
									Module, UrlParams, OwnerId, Token)
	end.

handle_req_method_op_not_ok(MethodToCall, Req, State, BodyPropList, 
							Module, UrlParams, OwnerId, Token) ->
	case MethodToCall of
		load_all ->
			handle_req_method_proceed(Req, State, load_all_for_public, BodyPropList, 
									  Module, UrlParams, OwnerId, Token);
		_ ->
			send_result(403, [rest_util:unauthorized_operation(MethodToCall)], Req, State, undefined, true)
	end.

handle_req_method_op_ok(MethodToCall, Req, State, BodyPropList, 
						Module, UrlParams, OwnerId, Token) ->
	case rest_validator:validate_ownership(MethodToCall, OwnerId, Token, Module, UrlParams) of
		true -> 
			handle_req_method_proceed(Req, State, MethodToCall, BodyPropList, 
									  Module, UrlParams, OwnerId, Token);
		_ ->
			Result = [rest_util:unauthorized_operation(MethodToCall)],
			Module:logoff(BodyPropList, Token),
			send_result(403, Result, Req, State, undefined, true)
	end.
	
handle_req_method_proceed(Req, State, MethodToCall, BodyPropList, Module, UrlParams, OwnerId, Token) ->
	case MethodToCall of
		special_op1 ->
			Result = Module:special_op1(BodyPropList, OwnerId, Token),
			Module:logoff(BodyPropList, Token),
			send_result(200, Result, Req, State, undefined, true);
		special_op2 ->
			Result = Module:special_op1(BodyPropList, OwnerId, Token),
			Module:logoff(BodyPropList, Token),
			send_result(200, Result, Req, State, undefined, true);
		save ->
			Result = rest_util:save(BodyPropList, OwnerId, Token, Module),
			Module:logoff(BodyPropList, Token),
			send_result(200, Result, Req, State, undefined, true);
		edit ->
	        GetSntp = generic_dataextractor:get_bool(BodyPropList, <<"getsntptimestamp">>, false),
			Language = generic_dataextractor:get_language(BodyPropList),
			Result = Module:edit(lists:nth(1, UrlParams)),
			Module:logoff(BodyPropList, Token),
			send_result(200, Result, Req, State, [GetSntp, Language], true);
		load_all ->
			handle_load(BodyPropList, MethodToCall, UrlParams, Module, OwnerId, Token, Req, State);
		load_all_for_public ->
			handle_load(BodyPropList, MethodToCall, UrlParams, Module, OwnerId, Token, Req, State);
		update ->
			Result = rest_util:update(BodyPropList, lists:nth(1, UrlParams), OwnerId, Token, Module),
			Module:logoff(BodyPropList, Token),
			send_result(200, Result, Req, State, undefined, true);
		delete ->
			Result = rest_util:delete(lists:nth(1, UrlParams), OwnerId, Token, Module),
			Module:logoff(BodyPropList, Token),
			send_result(200, Result, Req, State, undefined, true);
		restore ->
			Result = rest_util:restore(lists:nth(1, UrlParams), OwnerId, Token, Module),
			Module:logoff(BodyPropList, Token),
			send_result(200, Result, Req, State, undefined, true);
		trully_delete ->
			Result = rest_util:trully_delete(lists:nth(1, UrlParams), OwnerId, Token, Module),
			Module:logoff(BodyPropList, Token),
			send_result(200, Result, Req, State, undefined, true);
		_ ->
			Module:logoff(BodyPropList, Token),
			send_result(404, [rest_util:not_found(MethodToCall)], Req, State, undefined, true)
	end.

handle_load(BodyPropList, MethodToCall, UrlParams, Module, OwnerId, Token, Req, State) ->
	GetSntp = generic_dataextractor:get_bool(BodyPropList, <<"getsntptimestamp">>, false),
	Language = generic_dataextractor:get_language(BodyPropList),
	GetSntp2 = [GetSntp, Language],
	AddConditions = sessioncontrol_validator:get_additional_conditions_on_load(OwnerId, Token),
	Page = lists:nth(1, UrlParams),
	Rows = lists:nth(2, UrlParams),
	case MethodToCall of
		load_all ->
			Result = Module:load_all(BodyPropList, AddConditions, Page, Rows),
			Module:logoff(BodyPropList, Token),
			send_result(200, Result, Req, State, GetSntp2, false);
		_ -> 
			Result = Module:load_all_for_public(BodyPropList, AddConditions, Page, Rows),
			Module:logoff(BodyPropList, Token),
			send_result(200, Result, Req, State, GetSntp2, false)
	end.


