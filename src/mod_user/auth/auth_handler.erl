-module(auth_handler).

-export([init/2, login_auth_only/3, logoff_auth_only/2]).

init(Req, Opts) ->
	handle_request(Req, Opts).

login_auth_only(BodyPropList, Host, Token) ->
	Email = user_dataextractor:get_email(BodyPropList, ""),
	CanAuth = sessioncontrol_validator:can_authenticate(Email, Token),
	case ((Token == "") or (Email == "") or (CanAuth == false)) of
		true ->
			false;
		_ ->
			Password = user_dataextractor:get_password(BodyPropList, ""),
			Users = user_dao:load_for_login_first_access_or_confirmation(Email, Password, ""),
			case (length(Users) == 0) of
				true ->
					false;
				_ ->
					authenticate_user_auth_only(lists:nth(1, Users), Password, Token, Host)
			end
	end.

logoff_auth_only(BodyPropList, Token) ->
	Email = user_dataextractor:get_email(BodyPropList, ""),
	case ((Email == "") or (Token == "")) of
		true ->
			false;
		_ ->
			Users = user_dao:load_for_login_first_access_or_confirmation(Email, "", ""),
			(length(Users) > 0) and (sessioncontrol_validator:un_authenticate(Token))
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_request(Req0, State) ->
	Req = request_util:enable_rest_and_cors(Req0),
	Host = string_util:trim(request_util:get_host(Req)),
	BodyPropList = request_util:get_body(Req),
	Token = generic_dataextractor:get_token(BodyPropList),
	case ((Host == "") or (Token == "")) of
		true ->
			bad_request(Req, State);
		_ ->
			Method = cowboy_req:method(Req),
			Path = cowboy_req:path(Req),
			MethodToCallAndUrlParams = rest_service:parse_req_get_method(<<"/auth">>, Method, Path),
			case MethodToCallAndUrlParams of
				undefined ->
					bad_request(Req, State);
				_ ->
					[MethodToCall | _UrlParams] = MethodToCallAndUrlParams,
					case MethodToCall of
						undefined ->
							bad_request(Req, State);
						special_op1 ->
							login(BodyPropList, Host, Token, Req, State);
						special_op2 ->
							logoff(BodyPropList, Token, Req, State);
						_ ->
							bad_request(Req, State)
					end
			end
	end.

logoff(BodyPropList, Token, Req, State) ->
	Email = user_dataextractor:get_email(BodyPropList, ""),
	case (Email == "") of
		true ->
			rest_handler:send_result(200, [no_auth_user()], Req, State, undefined, false);
		_ ->
			Users = user_dao:load_for_login_first_access_or_confirmation(Email, "", ""),
			case ((length(Users) > 0) and (sessioncontrol_validator:un_authenticate(Token))) of
				true ->
					rest_handler:send_result(200, Users, Req, State, undefined, false);
				_ ->
					rest_handler:send_result(200, [[]], Req, State, undefined, false)
			end
	end.

login(BodyPropList, Host, Token, Req, State) ->
	Email = user_dataextractor:get_email(BodyPropList, ""),
	CanAuth = sessioncontrol_validator:can_authenticate(Email),
	case ((Email == "") or (CanAuth == false)) of
		true ->
			bad_request(Req, State);
		_ ->
			Password = user_dataextractor:get_password(BodyPropList),
			ConfirmationCode = user_dataextractor:get_confirmation_code(BodyPropList, ""),
			Users = user_dao:load_for_login_first_access_or_confirmation(Email, Password, ConfirmationCode),
			case (datatransform_util:is_emptylist(Users)) of
				true ->
					rest_handler:send_result(200, [no_auth_user()], Req, State, undefined, false);
				_ ->
					Result = authenticate_user(lists:nth(1, Users), Password, Token, Host, ConfirmationCode),
					rest_handler:send_result(200, [Result], Req, State, undefined, false)
			end
	end.

authenticate_user(User, Password, Token, Host, ConfirmationCode) ->
	ValidatedPassword = validate_password(User, Password),
	case (ValidatedPassword == false) of
		true ->
			no_auth_user();
		_ ->
			Inactive = number_util:to_integer(proplists:get_value(<<"active">>, User, 0)) == 0,
			case (Inactive or (ConfirmationCode /= "")) of
				true ->
					[{<<"password">>, Password} | proplists:delete(<<"password">>, User)];
				_ ->
					case (sessioncontrol_validator:set_authenticated(User, Token, Host)) of
						true ->
							[{<<"password">>, Password} | proplists:delete(<<"password">>, User)];
						_ ->
							no_auth_user()
					end
			end
	end.

authenticate_user_auth_only(User, Password, Token, Host) ->
	Inactive = number_util:to_integer(proplists:get_value(<<"active">>, User)) == 0,
	InvalidPassword = validate_password(User, Password) == false,
	if
		(Inactive or InvalidPassword) -> false;
		true -> sessioncontrol_validator:set_authenticated(User, Token, Host)
	end.

validate_password(User, Password) ->
	case datatransform_util:is_undefined(User) of
		true ->
			false;
		_ ->
			HashedPassword = string_util:trim(proplists:get_value(<<"password">>, User, "")),
			Matched = hash_util:password_match(HashedPassword, Password),
			(Password /= "") and (Matched == true)
	end.
		
bad_request(Req, State) ->
	rest_handler:send_result(400, [rest_util:bad_request()], Req, State, undefined, true).

no_auth_user() ->
	[{<<"id">>,0},{<<"name">>,<<"404">>}].

	