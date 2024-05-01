-module(dao_behaviour).

-callback get_database_name() -> 
	Res :: binary().

-callback load_object_by_id(Id :: integer()) -> 
	Res :: list().

-callback load_all_objects(Page :: integer(), 
                   Rows :: integer(), 
                   Conditions :: binary(), 
                   FlagAsDeleted :: boolean()) -> 
	Res :: list().

-callback load_all_objects_for_public(Page :: integer(), 
			                  Rows :: integer(), 
			                  Conditions :: binary(), 
			                  FlagAsDeleted :: boolean()) -> 
	Res :: list().

-callback delete_object(Id :: integer()) -> 
	Res :: boolean().

-callback restore_object(Id :: integer()) -> 
	Res :: boolean().

-callback trully_delete_object(Id :: integer()) -> 
	Res :: boolean().

-callback get_column_type(ColumnName ::binary()) ->
	Res :: binary().

