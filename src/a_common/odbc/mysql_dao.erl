-module(mysql_dao).

-export([insert_rows/3, select_rows/3, update_rows/3, delete_rows/3]).

select_rows(Sql, DatabaseName, Params) ->
	Pid = mysql_connection_pool:get_connection(DatabaseName),
	Result = mysql_dao_odbc:select(Pid, Sql, Params),
	mysql_connection_pool:unlock_config_conn(Pid),
	Result.

insert_rows(Sql, DatabaseName, Params) ->
	Pid = mysql_connection_pool:get_connection(DatabaseName),  
	Result = mysql_dao_odbc:insert(Pid, Sql, Params),
	mysql_connection_pool:unlock_config_conn(Pid),
	Result.

update_rows(Sql, DatabaseName, Params) ->
	Pid = mysql_connection_pool:get_connection(DatabaseName),  
	Result = mysql_dao_odbc:update(Pid, Sql, Params),
	mysql_connection_pool:unlock_config_conn(Pid),
	Result.

delete_rows(Sql, DatabaseName, Params) ->
	Pid = mysql_connection_pool:get_connection(DatabaseName),
	Result = mysql_dao_odbc:delete(Pid, Sql, Params),
	mysql_connection_pool:unlock_config_conn(Pid),
	Result.
	








