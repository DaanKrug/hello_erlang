-module(rest_util).

-export([
		 save/4,
		 update/5,
		 delete/4,
		 restore/4,
		 trully_delete/4,
		 bad_request/0,
		 unauthorized_operation/1,
		 not_found/1,
		 too_many_requests/0,
		 method_not_found/0
		]
	   ).

save(BodyPropList, OwnerId, Token, Module) -> 
	Validation = Module:validate_to_save(BodyPropList),
	case (datatransform_util:is_emptylist(Validation)) of
		true ->
			Module:save(BodyPropList, OwnerId, Token);
		_ ->
			Validation
	end.

update(BodyPropList, Id, OwnerId, Token, Module) -> 
	Validation = Module:validate_to_update(BodyPropList, Id),
	case (datatransform_util:is_emptylist(Validation)) of
		true ->
			Module:update(BodyPropList, Id, OwnerId, Token);
		_ ->
			Validation
	end.

delete(Id, OwnerId, Token, Module) -> 
	Validation = Module:validate_to_delete(Id),
	case (datatransform_util:is_emptylist(Validation)) of
		true ->
			Module:delete(Id, OwnerId, Token);
		_ ->
			Validation
	end.

restore(Id, OwnerId, Token, Module) -> 
	Validation = Module:validate_to_restore(Id),
	case (datatransform_util:is_emptylist(Validation)) of
		true ->
			Module:restore(Id, OwnerId, Token);
		_ ->
			Validation
	end.

trully_delete(Id, OwnerId, Token, Module) -> 
	Validation = Module:validate_to_trully_delete(Id),
	case (datatransform_util:is_emptylist(Validation)) of
		true ->
			Module:trully_delete(Id, OwnerId, Token);
		_ ->
			Validation
	end.

bad_request() ->
	[
	 {<<"objectClass">>, <<"Bad Request.">>}, 
     {<<"code">>, 400}, 
     {<<"msg">>, <<"Bad Request - One or More Invalid Parameters.">>} 
	].

unauthorized_operation(MethodToCall) -> 
	[
	 {<<"objectClass">>, iolist_to_binary(lists:concat(["Operation Not Authorized: ", MethodToCall]))}, 
     {<<"code">>, 403}, 
     {<<"msg">>, <<"">>} 
	].

not_found(MethodToCall) -> 
	[
	 {<<"objectClass">>, iolist_to_binary(lists:concat(["Operation Not Found: ", MethodToCall]))}, 
     {<<"code">>, 404}, 
     {<<"msg">>, <<"">>} 
	].

too_many_requests() -> 
	[
	 {<<"objectClass">>, <<"Too Many Requests.">>}, 
     {<<"code">>, 429}, 
     {<<"msg">>, <<"Too Many Requests.">>} 
	].

method_not_found() ->
	[
	 {<<"objectClass">>, <<"No Method Found.">>}, 
     {<<"code">>, 500}, 
     {<<"msg">>, <<"No Method Found.">>} 
	].