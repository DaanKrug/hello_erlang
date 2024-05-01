-module(sessioncontrol_dao).

-export(
   		[
		 get_database_name/0,  
		 get_column_type/1,
		 store_history_access/3, 
		 count_history_access/4, 
		 can_authenticate/1,
		 set_authenticated/4,
		 un_authenticate/1,
		 is_authenticated/3
		]
	   ).

get_database_name() ->
	lists:concat([base_dao:get_database_prefix_name(), "_session"]).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"userId">>),
	IsNumber2 = string_util:equals(ColumnName, <<"total">>),
	IsNumber3 = string_util:equals(ColumnName, <<"time">>),
	IsDate = string_util:equals(ColumnName, <<"created_at">>),
	IsDate2 = string_util:equals(ColumnName, <<"updated_at">>),
	IsDate3 = string_util:equals(ColumnName, <<"deleted_at">>),
	case (IsNumber or IsNumber2 or IsNumber3) of
		true ->
			"number";
		_ ->
			case (IsDate or IsDate2 or IsDate3) of
				true ->
					"date";
				_ -> 
					"binary"
			end
	end.

store_history_access(Host, Token, Resource) ->
	Now = date_util:time_to_sql_datetime(0, true),
	Sql0 = "update loggedusers set updated_at = ? where token = ? and ip = ?",
	Sql1 = "insert into accesscontrol(ip,token,resource,created_at) values(?,?,?,?)",
	base_dao:update(Sql0, [Now, Token, Host], ?MODULE),
	Result = base_dao:create(Sql1, [Host, Token, Resource, Now], ?MODULE),
	lists:nth(1, Result).

count_history_access(Host, Token, Resource, DateAfterOf) ->
    Sql = lists:concat([
						"select count(id) from accesscontrol where ip = ? "
					    "and token = ? and resource = ? and created_at >= ? "
					   ]),
	base_dao:load_count(Sql, [Host, Token, Resource, DateAfterOf], ?MODULE).
    
can_authenticate(Email) ->
	Sql0 = "select coalesce(amount,-1) as total from logintentatives where email = ? limit 1",
	Total = base_dao:load_count(Sql0, [Email], ?MODULE),
	case (Total >= 3) of
		true ->
			Sql1 = "select coalesce(time,0) as time from logintentatives where email = ? limit 1",
			Time = base_dao:load_count(Sql1, [Email], ?MODULE),
			case ((date_util:now(0) - Time) < 300000) of
				true ->
					false;
				_ ->
					set_times_try_login(Email, 1)
			end;
		_ ->
			case (Total > 0) of
				true ->
					set_times_try_login(Email, Total);
				_ ->
					set_first_try_login(Email)
			end
	end.
  
set_authenticated(Host, Token, Email, OwnerId) ->
    Sql0 = "delete from loggedusers where token = ? and userId = ?",
	base_dao:delete(Sql0, [Token, OwnerId], ?MODULE),
	Now = date_util:time_to_sql_datetime(0, true),
	Sql1 = "insert into loggedusers(token,userId,ip,created_at,updated_at) values(?,?,?,?,?)",
	Created = base_dao:create(Sql1, [Token, OwnerId, Host, Now, Now], ?MODULE),
	case (lists:nth(1, Created) == true) of
		true ->
			Sql2 = "delete from logintentatives where email = ?",
			base_dao:delete(Sql2, [Email], ?MODULE),
			true;
		_ ->
			false
	end.
  
un_authenticate(Token) ->
	Sql = "delete from loggedusers where token = ?",
    base_dao:delete(Sql, [Token], ?MODULE).
  
is_authenticated(Host, Token, OwnerId) ->
    Sql = "select userId from loggedusers where token = ? and userId = ? and ip = ? limit 1",
    ((base_dao:load_count(Sql, [Token, OwnerId, Host], ?MODULE)) > 0).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_times_try_login(Email, Times) ->
	Now0 = date_util:now(0),
	Now1 = date_util:time_to_sql_datetime(0, true),
    Sql = "update logintentatives set amount = ?, time = ?, updated_at = ? where email = ?",
    base_dao:update(Sql, [Times, Now0, Now1, Email], ?MODULE).
  
set_first_try_login(Email) ->
    Now0 = date_util:now(0),
	Now1 = date_util:time_to_sql_datetime(0, true),
    Sql = "insert into logintentatives(email,amount,time,created_at) values(?,?,?,?)",
	base_dao:create(Sql, [Email, 1, Now0, Now1], ?MODULE).
  
 