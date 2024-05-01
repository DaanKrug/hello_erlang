-module(base_dao_behaviour).

-callback get_database_prefix_name() ->
	Res :: binary().

-callback load_by_id(Sql ::binary(), 
                     Id :: integer(), 
                     Module :: module()) -> 
	Res :: list().

-callback create(Sql :: binary(), 
                 ParamValues :: list(), 
                 Module :: module()) -> 
	Res :: boolean().

-callback update(Sql :: binary(), 
                 ParamValues :: list(),
                 Module :: module()) ->
	Res :: boolean().

-callback load_all(Sql :: binary(),
                   Page :: integer(), 
                   Rows :: integer(), 
                   Conditions :: binary(), 
                   FlagAsDeleted :: boolean(),
				   OrderByCondition :: binary(),
				   Module :: module()) -> 
	Res ::list().

-callback delete(Sql ::binary(), 
                 Id :: integer(), 
                 Module :: module()) -> 
	Res :: boolean().

