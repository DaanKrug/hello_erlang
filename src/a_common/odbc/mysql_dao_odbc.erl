-module(mysql_dao_odbc).

-export([connect/1, disconnect/1, select/3, insert/3, update/3, delete/3]).

connect(ConnParams) ->
	case length(ConnParams) == 5 of
		true -> 
			connect(lists:nth(1, ConnParams),
					lists:nth(2, ConnParams),
					lists:nth(3, ConnParams),
					lists:nth(4, ConnParams));
		false ->
			connect(lists:nth(1, ConnParams),
					lists:nth(2, ConnParams),
					lists:nth(3, ConnParams),
					lists:nth(4, ConnParams),
					lists:nth(5, ConnParams))
	end.

connect(Hostname, Username, Password, DatabaseName) ->
	Config = [
				{host, Hostname}, 
				{user, Username},
				{password, Password}, 
				{database, DatabaseName}
			 ],
	{ok, Pid} = mysql:start_link(Config),
	Pid.

connect(Hostname, Username, Password, DatabaseName, SslCertPath) ->
	SslOpts = [
				{server_name_indication, disable},
				{cacertfile, SslCertPath}
			  ],
	Config = [
				{host, Hostname}, 
				{user, Username},
				{password, Password}, 
				{database, DatabaseName},
				{ssl, SslOpts}
			 ],
	{ok, Pid} = mysql:start_link(Config),
	Pid.

disconnect(Pid) ->
	mysql:stop(Pid).

select(Pid, Sql, Params) ->
	Sql2 = string_util:to_string(Sql),
	{ok, ColumnNames, Rows} = mysql:query(Pid, Sql2, Params),
	[ColumnNames, Rows].

insert(Pid, Sql, Params) ->
	ok = mysql:query(Pid, string_util:to_string(Sql), Params),
	LastInsertId = mysql:insert_id(Pid),
	AffectedRows = mysql:affected_rows(Pid),
	WarningCount = mysql:warning_count(Pid),
	[AffectedRows, WarningCount, LastInsertId].

update(Pid, Sql, Params) ->
	ok = mysql:query(Pid, string_util:to_string(Sql), Params),
	AffectedRows = mysql:affected_rows(Pid),
	WarningCount = mysql:warning_count(Pid),
	[AffectedRows, WarningCount].

delete(Pid, Sql, Params) ->
	ok = mysql:query(Pid, string_util:to_string(Sql), Params),
	AffectedRows = mysql:affected_rows(Pid),
	WarningCount = mysql:warning_count(Pid),
	[AffectedRows, WarningCount].
