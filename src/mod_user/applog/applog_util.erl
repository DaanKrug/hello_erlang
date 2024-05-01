-module(applog_util).

-export(
   		[
		 successfully_created/4, 
		 successfully_updated/5,
		 successfully_deleted/4,
		 successfully_restored/4,
		 successfully_trully_deleted/4
		]
	   ).

successfully_created(OwnerId, Object, ObjectTitle, Token) ->
    log_operation("Realizou Inclus&atilde;o", ObjectTitle, Object, undefined, OwnerId, Token).
  
successfully_updated(OwnerId, ObjectOld, ObjectNew, ObjectTitle, Token) ->
    log_operation("Realizou Altera&ccedil;&atilde;o", ObjectTitle, ObjectOld, ObjectNew, OwnerId, Token).
  
successfully_deleted(OwnerId, Object, ObjectTitle, Token) ->
    log_operation("Realizou Exclus&atilde;o", ObjectTitle, Object, undefined, OwnerId, Token).
  
successfully_restored(OwnerId, Object, ObjectTitle, Token) ->
    log_operation("Desfez Exclus&atilde;o", ObjectTitle, Object, undefined, OwnerId, Token).
  
successfully_trully_deleted(OwnerId, Object, ObjectTitle, Token) ->
    log_operation("Realizou Exclus&atilde;o Permanente", ObjectTitle, Object, undefined, OwnerId, Token).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_user(Id, Token) ->
	case string:find(Token, "_conferencist") of
		nomatch ->
			lists:nth(1, user_dao:load_object_by_id(Id));
		_ ->
			undefined
	end.

log_operation(OperationTitle, ObjectTitle, ObjectOld, ObjectNew, OwnerId, Token) ->
	User = get_user(OwnerId, Token),
	EmailKey = sessioncontrol_validator:get_email_key(Token),
	EncodedOld = case ObjectOld of
					 undefined ->
						 "";
					 _ ->
						 json_util:encode(maplist_util:parse([ObjectOld]))
				 end,
	EncodedNew = case ObjectNew of
					 undefined ->
						 "";
					 _ ->
						 json_util:encode(maplist_util:parse([ObjectNew]))
				 end,
	UserId = proplists:get_value(<<"id">>, User, ""),
	UserName = proplists:get_value(<<"name">>, User, ""),
	UserEmail = proplists:get_value(EmailKey, User, ""),
    applog_dao:create_object(UserId, UserName, UserEmail, OperationTitle, 
					         ObjectTitle, EncodedOld, EncodedNew).











