-module(mysql_connection_pool).

-export([
  setup/5,
  setup/6,
  get_connection/1,
  unlock_config_conn/1
]).

setup(Hostname, Username, Password, DatabaseName, MaxConn) ->
	setup([Hostname, Username, Password, DatabaseName, MaxConn], DatabaseName).

setup(Hostname, Username, Password, DatabaseName, SslCertPath, MaxConn) ->
	setup([Hostname, Username, Password, DatabaseName, SslCertPath, MaxConn], DatabaseName).

unlock_config_conn(Pid) ->
	ets_util:remove_from_cache(mysql_dao_ets, {locked, Pid}).

get_connection(DatabaseName) ->
	Key = {mysql_conn_params, DatabaseName, ?MODULE},
	ConnParams = ets_util:read_from_cache(mysql_dao_ets, Key),
	case (datatransform_util:is_undefined(ConnParams)) of
		true ->
			timer:sleep(1),
			get_connection(DatabaseName);
		_ ->
			get_connection2(DatabaseName, ConnParams)
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Private Functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup(ConnParams, DatabaseName) ->
	Key = {mysql_conn_params, DatabaseName, ?MODULE},
	Key2 = {next_mysql_conn_pool_number, DatabaseName, ?MODULE},
	ets_util:new(mysql_dao_ets, public, false, false),
	ets_util:store_in_cache(mysql_dao_ets, Key, ConnParams),
	ets_util:store_in_cache(mysql_dao_ets, Key2, 1),
	config_pool(ConnParams, 1, lists:last(ConnParams)).

config_pool(ConnParams, Count, MaxConn) -> 
	case (Count > MaxConn) of
		true -> 
			ok;
		false ->
			make_config_conn(Count, ConnParams),
			config_pool(ConnParams, Count + 1, MaxConn)
	end.

make_config_conn(Number, ConnParams) ->
	Key = {Number, mysql_conn, ConnParams},
	Pid = ets_util:read_from_cache(mysql_dao_ets, Key),
	case Pid of
        undefined -> 
			ok;
        _ -> 
			kill_config_conn(Number, ConnParams)
    end,
	Pid2 = mysql_dao_odbc:connect(ConnParams),
	ets_util:store_in_cache(mysql_dao_ets, Key, Pid2).

kill_config_conn(Number, ConnParams) ->
	Key = {Number, mysql_conn, ConnParams},
	Pid = ets_util:read_from_cache(mysql_dao_ets, Key),
	case Pid of
        undefined -> 
			ok;
        _ -> 
			mysql_dao_odbc:disconnect(Pid),
            ets_util:remove_from_cache(mysql_dao_ets, Key)
    end.
    
lock_config_conn(Pid) ->
	ets_util:store_in_cache(mysql_dao_ets, {locked, Pid}, 1).

is_locked_config_conn(Pid) ->
	undefined /= ets_util:read_from_cache(mysql_dao_ets, {locked, Pid}).

get_connection2(DatabaseName, ConnParams) ->
	MaxConn = lists:last(ConnParams),
	Key2 = {next_mysql_conn_pool_number, DatabaseName, ?MODULE},
	Next = ets_util:read_from_cache(mysql_dao_ets, Key2),
	Next2 = case (Next > MaxConn) of 
				true ->
					1;
				_ ->
					Next
			end,
	ets_util:store_in_cache(mysql_dao_ets, Key2, Next2 + 1),
	Key3 = {Next2, mysql_conn, ConnParams},
	Pid = ets_util:read_from_cache(mysql_dao_ets, Key3),
	case (datatransform_util:is_undefined(Pid) or is_locked_config_conn(Pid)) of
		true ->
			timer:sleep(1),
			get_connection(DatabaseName);
		_ -> 
			lock_config_conn(Pid),
			Pid
	end.
