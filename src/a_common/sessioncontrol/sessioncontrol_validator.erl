-module(sessioncontrol_validator).

-export(
   		[
		 store_history_access/3, 
		 validate_access_by_history_access/4, 
		 is_authenticated/3,
		 can_authenticate/1,
		 set_authenticated/3,
		 un_authenticate/1,
		 validate_ownership/4,
		 validate_access/4,
		 get_additional_conditions_on_load/2,
		 get_email_key/1
		]
	   ).

store_history_access(Host, Token, Resource) ->
  	sessioncontrol_handler:store_history_access_session(Host, Token, Resource).
  
validate_access_by_history_access(Host, Token, Resource, MaxTimesOnMinute) ->
	DateSeeAfter = date_util:time_to_sql_datetime(-1 * 60_000, false),
	TotalAccessTimes = sessioncontrol_handler:count_history_access_session(Host, Token, Resource, DateSeeAfter),
	TotalAccessTimes < MaxTimesOnMinute.

is_authenticated(Host, Token, OwnerId) ->
	sessioncontrol_handler:is_authenticated_session(Host, Token, OwnerId).

can_authenticate(Email) ->
	case validator:email(Email) of
		ok ->
			sessioncontrol_handler:can_authenticate_session(Email);
		_ ->
			false
	end.
  
set_authenticated(User, Token, Host) ->
	UserId = proplists:get_value(<<"id">>, User, 0),
    Email =  proplists:get_value(get_email_key(Token), User, ""),
	sessioncontrol_handler:set_authenticated_session(Host, Token, Email, UserId).
  
un_authenticate(Token) ->
    sessioncontrol_handler:un_authenticate_session(Token).
  
validate_ownership(OwnerId, Token, Module, Id) ->
	OwnerId2 = iolist_to_binary(integer_to_list(OwnerId)),
	Equals1 = Module:get_table_name() == get_table_name_auto_edit(Token),
	Equals2 = OwnerId2 == Id,
	if
		(Equals1 and Equals2) -> true;
		true ->
			Object = lists:nth(1, Module:edit(Id)),
			OwnerId == proplists:get_value(<<"ownerId">>, Object, 0)
	end.
 
validate_access(OwnerId, Token, Categories, Permission) ->
	Permission = get_permission_on_permission_groups(Permission),
	User = sessioncontrol_handler:load_for_permission_session(OwnerId, Token),
	Undefined1 = datatransform_util:is_undefined(User),
	Undefined2 = datatransform_util:is_undefined(Permission),
	if 
		(Undefined1 or Undefined2) -> false;
		true -> validate_user_access(User, Permission, Categories)
	end.

get_additional_conditions_on_load(OwnerId, Token) ->
    User = sessioncontrol_handler:load_for_permission_session(OwnerId, Token),
	case (datatransform_util:is_undefined(User)) of
		true ->
			"";
		_ ->
			Category = proplists:get_value(<<"category">>, User, ""),
		    OwnerOwnerId = proplists:get_value(<<"ownerId">>, User, 0),
			case Category of
				<<"enroll">> -> 
					lists:concat([" and ownerId in(", OwnerId, ",", OwnerOwnerId, ") "]);
				<<"admin">> -> 
					ChildIds = sessioncontrol_handler:load_child_ids_session(OwnerId),
					lists:concat([" and ownerId in(", OwnerId, ",", OwnerOwnerId, ",", ChildIds, ") "]);
				_ ->
					""
			end
	end.

get_email_key(Token) ->
	case string:find(Token, "_conferencist") of
		nomatch ->
			<<"email">>;
		_ ->
			<<"a2_email">>
	end.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_user_access(User, Permission, Categories) ->
	UserActive = proplists:get_value(<<"active">>, User, ""),
	case (number_util:to_integer(UserActive) == 1) of
		true ->
			UserCategory = proplists:get_value(<<"category">>, User, ""),
			case (UserCategory == <<"admin_master">>) of
				true ->
					true;
				_ ->
					Permissions = proplists:get_value(<<"permissions">>, User, ""),
					UserPermissions = string:split(Permissions, ",", all),
					IncludePermission = lists:member(Permission, UserPermissions),
					CategoriesIn = (length(Categories) == 0) or (lists:member(Categories, UserCategory)),
					IncludePermission and CategoriesIn
			end;
		_ ->
			false
	end.
	
get_table_name_auto_edit(Token) ->
	case string:find(Token, "_conferencist") of
		nomatch ->
			<<"user">>;
		_ ->
			<<"conconferencist">>
	end.

get_permission_on_permission_groups(Permission) ->
    Permission.
