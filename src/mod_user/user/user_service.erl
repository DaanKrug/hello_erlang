-module(user_service).

-export(
   		[
		 mail_from_contact_form/4,
		 mail_on_registered/2,
		 mail_on_recovered/3
		]
	   ).

mail_from_contact_form(Subject, Content, Name, Email) ->
	{_, TosAddress} = application:get_env(hello_erlang, admin_emails),
	Content1 = array_util:concat_all([
		Content, 
		"<br/> <div style=\"padding: .5em; border: 1px solid #ccc;\">",
		"<strong>Remetente:</strong>",
		Name, 
		" - ",
		Email,
		"</div>"
	]),
	simplemail_dao:create_object(Subject, Content1, TosAddress, string_util:generate_random(20), 0).

mail_on_registered(Email, ConfirmationCode) ->
	AppConfig = appconfig_dao:get_appconfig(),
    AppName = get_app_name(AppConfig),
    AppSite = get_app_site(AppConfig),
    Subject = array_util:concat_all(["Código de confirmação de acesso ", AppName]),
	Content = array_util:concat_all([
		"Utilize o código: <strong>",
		ConfirmationCode,
		"</strong><br/> para realizar sua confirmação de acesso em: <strong>",
		AppName, 
		"</strong>. <br/><br/> URL do site: ",
		"<span style=\"text-decoration: underline;color: #01f;\">",
		AppSite,
		"</span>. <br/><br/> O código de confirmação será solicitado ",
		"ao realizar o primeiro login. <br/> <br/>",
		get_msg_no_reply()
	]),
	simplemail_dao:create_object(Subject, Content, Email, string_util:generate_random(20), 0).

  
mail_on_recovered(Email, Password, ConfirmationCode) ->
	AppConfig = appconfig_dao:get_appconfig(), 
	AppName = get_app_name(AppConfig),
    AppSite = get_app_site(AppConfig),
    Subject = array_util:concat_all(["Nova Senha e Código de confirmação de acesso ", AppName]),
	Content = array_util:concat_all([
		"Utilize a nova senha: <strong>",
		Password,
		"</strong> <br/> e o novo código de confirmação: <strong>",
		ConfirmationCode,
		"</strong> <br/> para acessar em: <strong>",
		AppName,
		"</strong>.<br/><br/> URL do site: ",
		"<span style=\"text-decoration: underline; color: #01f;\">",
		AppSite,
		"</span>.<br/><br/> O código de confirmação será solicitado apenas",
		"no primeiro login após a solicitação de nova senha.<br/> <br/>",
		get_msg_no_reply()
	]),
	simplemail_dao:create_object(Subject, Content, Email, string_util:generate_random(20), 0).
 
get_msg_no_reply() ->
	array_util:concat_all([
		"<br/>===========================================================================",
		"<br/> Este é um email automático, por favor não responda.",
		"<br/>==========================================================================="
	]).
  
get_app_name(AppConfig) ->
	case datatransform_util:is_undefined(AppConfig) of
		true ->
			"App Base";
		_ ->
			proplists:get_value(<<"name">>, AppConfig)
	end.

get_app_site(AppConfig) ->
	case datatransform_util:is_undefined(AppConfig) of
		true ->
			"localhost";
		_ ->
			Site = string_util:trim(proplists:get_value(<<"site">>, AppConfig)),
			Site2 = string:replace(Site, "http://", ""),
			Site3 = string:replace(Site2, "https://", ""),
			Site4 = string:replace(Site3, "http:", ""),
			string:replace(Site4, "https:", "")
	end.