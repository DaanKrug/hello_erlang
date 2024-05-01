-module(sntp_dao).

-export(
   		[
		 get_database_name/0,  
		 get_column_type/1,
		 load_last_diff_by_language/1
		]
	   ).

get_database_name() ->
	lists:concat([base_dao:get_database_prefix_name(), "_session"]).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"systemdiff">>),
	case (IsNumber or IsNumber2) of
		true ->
			"number";
		_ ->
			"binary"
	end.

load_last_diff_by_language(Language) ->
	Sql = "select systemdiff from timesync where language = ? order by id desc limit 1",
	base_dao:load_count(Sql, [Language], ?MODULE).
  











