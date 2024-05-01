-module(hello_erlang_app).

-behaviour(application).

-import(mysql_connection_pool,[setup/5]).

-export([start/2, stop/1]).

-define(APP_NAME, hello_erlang).
-define(CONFIG,[{port, 8080}]).
% -define(SSL_CONFIG,[{port, 8443}, {certfile, "/path/to/certfile"}, {keyfile, "/path/to/keyfile"}]).
-define(HTML_MIME_TYPE,[{mimetypes, {<<"text">>, <<"html">>, []}}]).
-define(STATIC_FILES_CONFIG, [{mimetypes, cow_mimetypes, all}, {dir_handler, directory_handler}]).

start(_Type, _Args) ->
  setup_databases(),
  register_routes(),
	{ok, _} = cowboy:start_clear(http, ?CONFIG, env_opts()),
	%{ok, _} = cowboy:start_tls(my_https_listener, ?SSL_CONFIG, env_opts()),
	start_erlcloud_to_aws(),
	hello_erlang_sup:start_link().
	
stop(_State) ->
	ok = cowboy:stop_listener(http).

setup_databases() ->
	mysql_connection_pool:setup("localhost","echo","123456", "erlang", 1),
	mysql_connection_pool:setup("localhost","echo","123456", "erlang_log", 1),
	mysql_connection_pool:setup("localhost","echo","123456", "erlang_config", 1),
	mysql_connection_pool:setup("localhost","echo","123456", "erlang_session", 1),
	mysql_connection_pool:setup("localhost","echo","123456", "erlang_queue", 1),
	mysql_connection_pool:setup("localhost","echo","123456", "erlang_billingcontrol", 1),
	mysql_connection_pool:setup("localhost","echo","123456", "erlang_coin", 1),
	io:format("databases config ok ..."),
	ok.


register_routes() ->
	persistent_term:erase(my_app_dispatch),
	Dispatch = cowboy_router:compile(get_routes()),
	persistent_term:put(my_app_dispatch, Dispatch).

env_opts() ->
	#{env => #{
			   dispatch => persistent_term:get(my_app_dispatch), 
			   middlewares => [cowboy_router, directory_listener, cowboy_handler]
			  }
	}.

start_erlcloud_to_aws() -> 
	ssl:start(), % https://github.com/erlcloud/erlcloud
	lhttpc:start(), % https://github.com/erlcloud/erlcloud
	s3config_dao:bootstrap_s3_config().
    
get_routes() -> 
	Params1 = [{id, int}],
	Params2 = [{page, int}, {rows, int}],
	StaticParams = {priv_dir, ?APP_NAME, "static/", ?STATIC_FILES_CONFIG},
	NotFoundParams = {priv_file, ?APP_NAME, "static/404.html",?HTML_MIME_TYPE},
	[
	 	{'_',
			[
				{"/hello_erlang/auth", auth_handler, []},
				{"/hello_erlang/awsrekognition", awsrekognition_handler, []},
				{"/hello_erlang/cepsearch", cepsearch_handler, []},
				{"/hello_erlang/contact_message", usercontact_handler, []},
				{"/hello_erlang/user_activate", useractivator_handler, []},
				{"/hello_erlang/user_recover", userrecover_handler, []},
				{"/hello_erlang/pdf", pdf_handler, []},
				{"/hello_erlang/appconfigs", appconfig_handler, []},
				{"/hello_erlang/appconfigs/:id", Params1, appconfig_handler, []},
				{"/hello_erlang/appconfigs/unDrop/:id", Params1, appconfig_handler, []},
        {"/hello_erlang/appconfigs/trullyDrop/:id", Params1, appconfig_handler, []},
				{"/hello_erlang/appconfigs/:page/:rows", Params2, appconfig_handler, []},
				{"/hello_erlang/applogs", applog_handler, []},
				{"/hello_erlang/applogs/:id", Params1, applog_handler, []},
				{"/hello_erlang/applogs/:page/:rows", Params2, applog_handler, []},
				{"/hello_erlang/appthemes", apptheme_handler, []},
				{"/hello_erlang/appthemes/:id", Params1, apptheme_handler, []},
				{"/hello_erlang/appthemes/unDrop/:id", Params1, apptheme_handler, []},
				{"/hello_erlang/appthemes/trullyDrop/:id", Params1, apptheme_handler, []},
				{"/hello_erlang/appthemes/:page/:rows", Params2, apptheme_handler, []},
				{"/hello_erlang/externalsimplemails", externalsimplemail_handler, []},
				{"/hello_erlang/files", file_handler, []},
				{"/hello_erlang/files/:id", Params1, file_handler, []},
				{"/hello_erlang/files/unDrop/:id", Params1, file_handler, []},
        {"/hello_erlang/files/trullyDrop/:id", Params1, file_handler, []},
				{"/hello_erlang/files/:page/:rows", Params2, file_handler, []},
				{"/hello_erlang/itaubankconfigs", itaubankconfig_handler, []},
				{"/hello_erlang/itaubankconfigs/:id", Params1, itaubankconfig_handler, []},
				{"/hello_erlang/itaubankconfigs/unDrop/:id", Params1, itaubankconfig_handler, []},
				{"/hello_erlang/itaubankconfigs/trullyDrop/:id", Params1, itaubankconfig_handler, []},
				{"/hello_erlang/itaubankconfigs/:page/:rows", Params2, itaubankconfig_handler, []},
				{"/hello_erlang/masternodes", masternode_handler, []},
				{"/hello_erlang/masternodes/:id", Params1, masternode_handler, []},
				{"/hello_erlang/masternodes/unDrop/:id", Params1, masternode_handler, []},
				{"/hello_erlang/masternodes/trullyDrop/:id", Params1, masternode_handler, []},
				{"/hello_erlang/masternodes/:page/:rows", Params2, masternode_handler, []},
				{"/hello_erlang/mailerconfigs", mailerconfig_handler, []},
				{"/hello_erlang/mailerconfigs/:id", Params1, mailerconfig_handler, []},
				{"/hello_erlang/mailerconfigs/unDrop/:id", Params1, mailerconfig_handler, []},
				{"/hello_erlang/mailerconfigs/trullyDrop/:id", Params1, mailerconfig_handler, []},
				{"/hello_erlang/mailerconfigs/:page/:rows", Params2, mailerconfig_handler, []},
				{"/hello_erlang/modules", module_handler, []},
				{"/hello_erlang/modules/:id", Params1, module_handler, []},
				{"/hello_erlang/modules/unDrop/:id", Params1, module_handler, []},
				{"/hello_erlang/modules/trullyDrop/:id", Params1, module_handler, []},
				{"/hello_erlang/modules/:page/:rows", Params2, module_handler, []},
				{"/hello_erlang/pagemenus", pagemenu_handler, []},
				{"/hello_erlang/pagemenus/:id", Params1, pagemenu_handler, []},
				{"/hello_erlang/pagemenus/unDrop/:id", Params1, pagemenu_handler, []},
				{"/hello_erlang/pagemenus/trullyDrop/:id", Params1, pagemenu_handler, []},
				{"/hello_erlang/pagemenus/:page/:rows", Params2, pagemenu_handler, []},
				{"/hello_erlang/pagemenuitems", pagemenuitem_handler, []},
				{"/hello_erlang/pagemenuitems/:id", Params1, pagemenuitem_handler, []},
				{"/hello_erlang/pagemenuitems/unDrop/:id", Params1, pagemenuitem_handler, []},
				{"/hello_erlang/pagemenuitems/trullyDrop/:id", Params1, pagemenuitem_handler, []},
				{"/hello_erlang/pagemenuitems/:page/:rows", Params2, pagemenuitem_handler, []},
				{"/hello_erlang/pagemenuitemfiles", pagemenuitemfile_handler, []},
				{"/hello_erlang/pagemenuitemfiles/:id", Params1, pagemenuitemfile_handler, []},
				{"/hello_erlang/pagemenuitemfiles/unDrop/:id", Params1, pagemenuitemfile_handler, []},
				{"/hello_erlang/pagemenuitemfiles/trullyDrop/:id", Params1, pagemenuitemfile_handler, []},
				{"/hello_erlang/pagemenuitemfiles/:page/:rows", Params2, pagemenuitemfile_handler, []},
				{"/hello_erlang/s3configs", s3config_handler, []},
				{"/hello_erlang/s3configs/:id", Params1, s3config_handler, []},
				{"/hello_erlang/s3configs/unDrop/:id", Params1, s3config_handler, []},
				{"/hello_erlang/s3configs/trullyDrop/:id", Params1, s3config_handler, []},
				{"/hello_erlang/s3configs/:page/:rows", Params2, s3config_handler, []},
				{"/hello_erlang/simplemails", simplemail_handler, []},
				{"/hello_erlang/simplemails/:id", Params1, simplemail_handler, []},
				{"/hello_erlang/simplemails/unDrop/:id", Params1, simplemail_handler, []},
				{"/hello_erlang/simplemails/trullyDrop/:id", Params1, simplemail_handler, []},
				{"/hello_erlang/simplemails/:page/:rows", Params2, simplemail_handler, []},
				{"/hello_erlang/users", user_handler, []},
				{"/hello_erlang/users/:id", Params1, user_handler, []},
				{"/hello_erlang/users/unDrop/:id", Params1, user_handler, []},
				{"/hello_erlang/users/trullyDrop/:id", Params1, user_handler, []},
				{"/hello_erlang/users/:page/:rows", Params2, user_handler, []},
				{"/hello_erlang/wallets", wallet_handler, []},
				{"/hello_erlang/wallets/:id", Params1, wallet_handler, []},
				{"/hello_erlang/wallets/unDrop/:id", Params1, wallet_handler, []},
        {"/hello_erlang/wallets/trullyDrop/:id", Params1, wallet_handler, []},
				{"/hello_erlang/wallets/:page/:rows", Params2, wallet_handler, []},
				{"/hello_erlang/wallettransactions", wallettransaction_handler, []},
				{"/hello_erlang/wallettransactions/:id", Params1, wallettransaction_handler, []},
				{"/hello_erlang/wallettransactions/unDrop/:id", Params1, wallettransaction_handler, []},
        {"/hello_erlang/wallettransactions/trullyDrop/:id", Params1, wallettransaction_handler, []},
				{"/hello_erlang/wallettransactions/:page/:rows", Params2, wallettransaction_handler, []},
				{"/hello_erlang/[...]", cowboy_static, StaticParams},
				{'_', cowboy_static, NotFoundParams}
			]
		}
    ].

