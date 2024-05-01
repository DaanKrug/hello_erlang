-module(sessioncontrol_handler).

-export(
   		[
		 store_history_access_session/3, 
		 count_history_access_session/4, 
		 is_authenticated_session/3,
		 can_authenticate_session/1,
		 set_authenticated_session/4,
		 un_authenticate_session/1,
		 load_for_permission_session/2,
		 load_child_ids_session/1
		]
	   ).

store_history_access_session(Host, Token, Resource) ->
	sessioncontrol_dao:store_history_access(Host, Token, Resource).

count_history_access_session(Host, Token, Resource, DateAfterOf) ->
	sessioncontrol_dao:count_history_access(Host, Token, Resource, DateAfterOf).

is_authenticated_session(Host, Token, OwnerId) ->
	sessioncontrol_dao:is_authenticated(Host, Token, OwnerId).

can_authenticate_session(Email) ->
	sessioncontrol_dao:can_authenticate(Email).

set_authenticated_session(Host, Token, Email, OwnerId) ->
	sessioncontrol_dao:set_authenticated(Host, Token, Email, OwnerId).

un_authenticate_session(Token) ->
	sessioncontrol_dao:un_authenticate(Token).

load_for_permission_session(OwnerId, Token) ->
	if
		(OwnerId < 1) -> undefined;
		true ->
			case (string:find(Token, "_conferencist")) of
				nomatch ->
					Users = user_dao:load_object_by_id(OwnerId),
					case (length(Users) == 0) of
						true ->
							undefined;
						_ -> 
							lists:nth(1, Users)
					end;
				_ ->
					%load a conferencist;
					undefined
			end
	end.

load_child_ids_session(OwnerId) ->
	Result = user_dao:load_user_ids_of_owner(OwnerId),
	proplists:get_value(<<"ids">>, Result, "").





