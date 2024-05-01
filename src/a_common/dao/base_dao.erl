-module(base_dao).

-behaviour(base_dao_behaviour).

-export(
   		[
		 get_database_prefix_name/0,
		 load_by_id/3, 
		 load_by_params/3, 
		 create/3, 
		 update/3, 
		 delete/3, 
		 load_all/7, 
		 load_count/3
		]
	   ).

get_database_prefix_name() ->
	"erlang".

load_by_id(Sql, Id, Module) -> 
	load_by_params(Sql, [Id], Module).

load_by_params(Sql, ParamValues, Module) -> 
	Resultset = mysql_dao:select_rows(Sql, Module:get_database_name(), ParamValues),
	TotalRows = length(lists:nth(2, Resultset)),
	parse_results(Resultset, TotalRows, Module).

create(Sql, ParamValues, Module) -> 
	Resultset = mysql_dao:insert_rows(Sql, Module:get_database_name(), ParamValues),
	AffectedRows = lists:nth(1, Resultset),
	InsertedId = lists:nth(3, Resultset),
	[AffectedRows > 0, InsertedId].

update(Sql, ParamValues, Module) -> 
	Resultset = mysql_dao:update_rows(Sql, Module:get_database_name(), ParamValues),
	AffectedRows = lists:nth(1, Resultset),
	AffectedRows > 0.

delete(Sql, ParamValues, Module) -> 
	Resultset = mysql_dao:delete_rows(Sql, Module:get_database_name(), ParamValues),
	AffectedRows = lists:nth(1, Resultset),
	AffectedRows > 0.

load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, Module) ->
	Sql2 = concat_sql(Sql, FlagAsDeleted, Conditions, OrderByCondition, Page, Rows),
	Resultset = mysql_dao:select_rows(Sql2, Module:get_database_name(), []),
	parse_results(Resultset, get_total_rows(Sql, Conditions, FlagAsDeleted, Module), Module).

load_count(Sql, ParamValues, Module) ->
	Resultset = mysql_dao:select_rows(Sql, Module:get_database_name(), ParamValues),
	get_count_result(Resultset).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_total_rows(Sql, Conditions, FlagAsDeleted, Module) ->
	Sql2 = concat_sql_for_total(Sql, FlagAsDeleted, Conditions),
	Resultset = mysql_dao:select_rows(Sql2, Module:get_database_name(), []),
	get_count_result(Resultset).

get_count_result(Resultset) ->
	Result = lists:nth(2, Resultset),
	case (datatransform_util:is_emptylist(Result)) of
		true -> 
			0;
		_ ->
			Count = lists:nth(1, lists:nth(1, Result)),
			number_util:to_integer(Count)
	end.

concat_sql(Sql, FlagAsDeleted, Conditions, OrderByCondition, Page, Rows) ->
	lists:concat([
				  string_util:to_list(Sql),
			      get_deleted_at(FlagAsDeleted),
				  " ",
				  string_util:to_list(string_util:to_string(Conditions)),
				  " ",
			      get_order_by(OrderByCondition),
				  " ",
				  get_limit_rows(Page, Rows),
				  ";"
			      ]).

concat_sql_for_total(Sql, FlagAsDeleted, Conditions) ->
	lists:concat([
				  "select count(id) ", 
				  " ",
				  string_util:to_list(string:find(Sql, " from ")),
				  " ",
				  get_deleted_at(FlagAsDeleted),
				  " ",
				  string_util:to_list(string_util:to_string(Conditions)),
				  ";"
			     ]).

parse_results(Resultset, TotalRows, Module) ->
	ColumnNames = lists:nth(1, Resultset),
	Rows = lists:nth(2, Resultset),
	ColumnTypes = get_column_types(ColumnNames, Module),
	case (TotalRows > 0) of
		true -> 
			get_proplist_result(ColumnNames, ColumnTypes, Rows, TotalRows);
		_ ->
			get_empty_proplist_result(ColumnNames, ColumnTypes)
	end.

get_column_types(ColumnNames, Module) ->
	Length = length(ColumnNames),
	get_column_types(ColumnNames, [], 1, Length, Module).

get_column_types(ColumnNames, ColumnTypes, Count, Length, Module) ->
	case (Count > Length) of 
		true -> 
			lists:reverse(ColumnTypes);
		_ ->
			Type = Module:get_column_type(lists:nth(Count, ColumnNames)),
			get_column_types(ColumnNames, [Type | ColumnTypes], Count + 1, Length, Module)
	end.

get_proplist_result(ColumnNames, ColumnTypes, Rows, TotalRows) ->
	Length = length(Rows),
	get_proplist_result2([], ColumnNames, ColumnTypes, Rows, 1, Length, TotalRows).

get_proplist_result2(PropList, ColumnNames, ColumnTypes, Rows, Count, Length, TotalRows) ->
	case (Count > Length) of 
		true ->
			[Header | Tail] = lists:reverse(PropList),
			Header2 = [{<<"totalRows">>, TotalRows} | Header],
			[Header2 | Tail];
		_ -> 
			PropListObjectRow = proplist_util:put(ColumnNames, ColumnTypes, lists:nth(Count, Rows), []),
			PropListObjectRow2 = [PropListObjectRow | PropList], 
			get_proplist_result2(PropListObjectRow2, ColumnNames, ColumnTypes, Rows, Count + 1, Length, TotalRows)
	end.

get_empty_proplist_result(ColumnNames, ColumnTypes) ->
	[proplist_util:put(ColumnNames, ColumnTypes, undefined, [{totalRows, 0}])].

get_deleted_at(FlagAsDeleted) ->
	case (FlagAsDeleted == true) of
		true -> 
			" where deleted_at > '0000-00-00 00:00:00' ";
		_ -> 
			" where (deleted_at = '0000-00-00 00:00:00' or deleted_at is null) "
	end.

get_order_by(OrderByCondition) ->
	case OrderByCondition of
		undefined -> 
			" order by id asc ";
		"" ->
			" order by id asc ";
		_ -> 
			OrderByCondition
	end.
	
get_limit_rows(Page, Rows) ->
	Page2 = number_util:to_integer(Page),
	Rows2 = number_util:to_integer(Rows),
	case ((Page2 > 0) and (Rows2 > 0)) of 
		true -> 
			lists:concat([" limit ", ((Page2 - 1) * Rows2), ",", Rows2]);
		_ -> 
			""
	end.

