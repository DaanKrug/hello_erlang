-module(handler_behaviour).

-callback object_class_name() ->
	Res :: binary().

-callback get_base_url() -> 
	Res :: binary().

-callback get_max_access_times_on_minute() ->
	Res ::integer().

-callback get_categories() -> 
	Res :: list().

-callback get_table_name() -> 
	Res :: binary().

-callback authorize_operation(BodyPropList :: list(), UrlParams :: list(), MethodToCall :: binary()) ->
	Res :: boolean().

-callback need_auth(BodyPropList :: list(), MethodToCall :: binary()) ->
	Res :: boolean().

-callback skip_validate_ownership(MethodToCall :: binary(), OwnerId :: number(), 
                                  Token :: binary(), UrlParams :: list()) ->
	Res :: boolean().

-callback validate_to_save(BodyPropList :: list()) -> 
	Res :: list().

-callback validate_to_update(BodyPropList :: list(), Id :: binary()) -> 
	Res :: list().

-callback validate_to_delete(Id :: binary()) -> 
	Res :: list().

-callback validate_to_restore(Id :: binary()) -> 
	Res :: list().

-callback validate_to_trully_delete(Id :: binary()) -> 
	Res :: list().

-callback save(BodyPropList :: list(), OwnerId :: binary(), Token :: binary()) -> 
	Res :: list().

-callback edit(Id :: binary()) -> 
	Res :: list().

-callback load_all(BodyPropList :: list(), AdditionalConditions :: binary(), Page :: binary(), Rows :: binary()) -> 
	Res :: list().

-callback load_all_for_public(BodyPropList :: list(), AdditionalConditions :: binary(), Page :: binary(), Rows :: binary()) -> 
	Res :: list().

-callback update(BodyPropList :: list(), Id :: binary(), OwnerId :: binary(), Token :: binary()) -> 
	Res :: list().

-callback delete(Id :: binary(), OwnerId :: binary(), Token :: binary()) -> 
	Res :: list().

-callback restore(Id :: binary(), OwnerId :: binary(), Token :: binary()) -> 
	Res :: list().

-callback trully_delete(Id :: binary(), OwnerId :: binary(), Token :: binary()) -> 
	Res :: list().

-callback authenticate(BodyPropList :: list(), Host :: binary(), Token :: binary()) ->
	Res :: boolean().

-callback logoff(BodyPropList :: list(), Token :: binary()) ->
    Res :: boolean().

-callback special_op1(BodyPropList :: list(), OwnerId :: binary(), Token :: binary()) ->
	Res :: list().

-callback special_op2(BodyPropList :: list(), OwnerId :: binary(), Token :: binary()) ->
	Res :: list().









