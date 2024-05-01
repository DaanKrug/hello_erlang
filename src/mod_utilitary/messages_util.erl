-module(messages_util).

-export([system_message/1, system_message/2]).

system_message(MessageCode, Parameters) ->
	case (datatransform_util:is_emptylist(Parameters)) of
		false ->
			ParamValue = lists:nth(1, Parameters),
			if
				(MessageCode == 209) -> return_util:get_operation_success(209, ParamValue);
		        (MessageCode == 210) -> return_util:get_operation_success(210, ParamValue);
		        (MessageCode == 211) -> return_util:get_operation_success(211, ParamValue);
		        (MessageCode == 212) -> return_util:get_operation_success(212, ParamValue);
				(MessageCode == 480) -> return_util:get_validation_result(480,
									      lists:concat([
														"Preencher corretamente os campos requeridos ", 
														"para criar novo(a) <strong>",
														ParamValue, 
														"</strong>."
													   ]));
				(MessageCode == 100000) -> return_util:get_validation_result(100000,
							                 lists:concat([
														   "J&aacute; existe uma pessoa/usu&aacute;rio com o email:<strong>",
														   ParamValue,
														   "</strong>"
														  ]))
			end;
		_ ->
			system_message(MessageCode)
	end.

system_message(MessageCode) ->
    if
      (MessageCode == 0) -> return_util:get_operation_error("Erro de opera&ccedil;&atilde;o.");
      (MessageCode == 200) -> return_util:get_operation_success(200, "");
      (MessageCode == 201) -> return_util:get_operation_success(201, "");
      (MessageCode == 204) -> return_util:get_operation_success(204, "");
      (MessageCode == 205) -> return_util:get_validation_result(205, "OK");
      (MessageCode == 207) -> return_util:get_operation_success(207, "");
      (MessageCode == 208) -> return_util:get_operation_success(208, "");
      (MessageCode == 209) -> return_util:get_operation_success(209, "");
      (MessageCode == 210) -> return_util:get_operation_success(210, "");
      (MessageCode == 211) -> return_util:get_operation_success(211, "");
      (MessageCode == 212) -> return_util:get_operation_success(212, "");
      (MessageCode == 403) -> return_util:get_operation_error("Falta de permiss&atilde;o de acesso ao recurso.");
	  (MessageCode == 412) -> return_util:get_operation_error("Falha de pr&eacute; condi&ccedil;&atilde;o.");
	  (MessageCode == 413) -> return_util:get_operation_error("[TESTE] Falha de pr&eacute; condi&ccedil;&atilde;o.");
	  (MessageCode == 414) -> return_util:get_operation_error("Erro ao gerar arquivo PDF.");
	  (MessageCode == 415) -> return_util:get_operation_error("Erro ao gerar arquivo PDF.");
      (MessageCode == 100000) -> return_util:get_validation_result(100000,
                                   "J&aacute; existe uma pessoa/usu&aacute;rio com o email informado.");
      (MessageCode == 100001) -> return_util:get_validation_result(100001,
						           lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
						                         "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o."
												]));
      (MessageCode == 100003) -> return_util:get_validation_result(100003,
                                   "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o pode ser exclu&iacute;da.");
      (MessageCode == 100004) -> return_util:get_validation_result(100004,
                                   lists:concat([
												 "Obrigat&oacute;rio informar corretamente os campos ",
												 "<strong>Nome</strong>, <strong>E-mail</strong> ",
												 "e <strong>Senha</strong>."
												]));
      (MessageCode == 100005) -> return_util:get_validation_result(100005,
                                   "Falha ao criar <strong>pessoa/usu&aacute;rio</strong>.");
      (MessageCode == 100006) -> return_util:get_validation_result(100006,
                                   "Falha ao efetuar <strong>registro</strong>.");
      (MessageCode == 100007) -> return_util:get_validation_result(100007,
                                   "Falha ao alterar <strong>pessoa/usu&aacute;rio</strong>.");
      (MessageCode == 100008) -> return_util:get_validation_result(100008,
                                   "N&atilde;o foi poss&iacute;vel ativar <strong>usu&aacute;rio</strong>.");
      (MessageCode == 100009) -> return_util:get_validation_result(100009,
				                   "Falha ao ativar <strong>usu&aacute;rio</strong>.");
      (MessageCode == 100011) -> return_util:get_validation_result(100011,
								   "Falha ao recuperar <strong>senha</strong>.");
      (MessageCode == 100014) -> return_util:get_validation_result(100014,
                                   "<strong>E-mail</strong> em processamento, n&atilde;o pode ser alterado.");
      (MessageCode == 100015) -> return_util:get_validation_result(100015,
                                   "<strong>E-mail</strong> em processamento, n&atilde;o pode ser exclu&iacute;do.");
      (MessageCode == 100016) -> return_util:get_validation_result(100016,
								   "Falha ao criar <strong>e-mail</strong>.");
      (MessageCode == 100017) -> return_util:get_validation_result(100017,
                                   "Falha ao alterar <strong>e-mail</strong>.");
      (MessageCode == 100018) -> return_util:get_validation_result(100018,
                                   lists:concat([
												 "<strong>E-mail</strong> j&aacute; enviado para 1 ou mais ",
												 " destinat&aacute;rios, n&atilde;o pode ser alterado."
												]));
      (MessageCode == 100019) -> return_util:get_validation_result(100019,
                                   "O e-mail informado, n&atilde;o se encaixa nas regras para novo registro.");
      (MessageCode == 100023) -> return_util:get_validation_result(100023,
                                   lists:concat([
												 "Obrigat&oacute;rio informar corretamente os campos ",
												 "<strong>Provedor</strong>, <strong>Identifica&ccedil;&atilde;o</strong>, ",
												 "<strong>Login/Endere&ccedil;o de E-mail</strong>,",
												 " <strong>Senha</strong> e <strong>Replay-To</strong>."
												]));
      (MessageCode == 100024) -> return_util:get_validation_result(100024,
                                   "Falha ao criar <strong>configura&ccedil;&atilde;o envio e-mail</strong>.");
      (MessageCode == 100025) -> return_util:get_validation_result(100025,
                                   "Falha ao alterar <strong>configura&ccedil;&atilde;o envio e-mail</strong>.");
      (MessageCode == 100026) -> return_util:get_validation_result(100026,
                                   lists:concat([
												 "Obrigat&oacute;rio informar corretamente os campos ",
												 "<strong>Identifica&ccedil;&atilde;o</strong> e <strong>Link</strong>."
												]));
      (MessageCode == 100027) -> return_util:get_validation_result(100027,
                                   "Falha ao criar <strong>arquivo</strong>.");
      (MessageCode == 100028) -> return_util:get_validation_result(100028,
								   "Falha ao alterar <strong>arquivo</strong>.");
      (MessageCode == 100029) -> return_util:get_validation_result(100029,
						           "Falha ao criar <strong>imagem</strong>.");
      (MessageCode == 100030) -> return_util:get_validation_result(100030,
								   "Falha ao alterar <strong>imagem</strong>.");
      (MessageCode == 100031) -> return_util:get_validation_result(100031,
                                 "Obrigat&oacute;rio informar campo <strong>Identifica&ccedil;&atilde;o</strong>.");
      (MessageCode == 100032) -> return_util:get_validation_result(100032,
								   "Falha ao criar <strong>menu</strong>.");
      (MessageCode == 100033) -> return_util:get_validation_result(100033,
								   "Falha ao alterar <strong>menu</strong>.");
      (MessageCode == 100034) -> return_util:get_validation_result(100034,
								   "<strong>Menu</strong> informado n&atilde;o existe.");
      (MessageCode == 100035) -> return_util:get_validation_result(100035,
                                   lists:concat([
												 "Obrigat&oacute;rio informar corretamente os campos ",
												 "<strong>Identifica&ccedil;&atilde;o</strong> e <strong>Conte&uacute;do</strong>."
												]));
      (MessageCode == 100036) -> return_util:get_validation_result(100036,
                                   "Falha ao criar <strong>item menu</strong>.");
      (MessageCode == 100037) -> return_util:get_validation_result(100037,
                                   "Falha ao alterar <strong>item menu</strong>.");
      (MessageCode == 100038) -> return_util:get_validation_result(100038,
                                   "<strong>Item menu</strong> informado n&atilde;o existe.");
      (MessageCode == 100039) -> return_util:get_validation_result(100039,
                                   "Falha ao criar <strong>arquivo item menu</strong>.");
      (MessageCode == 100040) -> return_util:get_validation_result(100040,
                                   "Falha ao alterar <strong>arquivo item menu</strong>.");
      (MessageCode == 100041) -> return_util:get_validation_result(100041,
                                   "<strong>Arquivo</strong> informado n&atilde;o existe.");
      (MessageCode == 100042) -> return_util:get_validation_result(100042,
                                   "<strong>Arquivo</strong> informado j&aacute; est&aacute; adicionado ao Item Menu.");
      (MessageCode == 100047) -> return_util:get_validation_result(100047,
                                   "Falha ao criar <strong>m&oacute;dulo</strong>.");
      (MessageCode == 100048) -> return_util:get_validation_result(100048,
                                   "Falha ao alterar <strong>m&oacute;dulo</strong>.");
      (MessageCode == 100049) -> return_util:get_validation_result(100049,
                                   "<strong>M&oacute;dulo</strong> informado j&aacute; est&aacute; adicionado na aplica&ccedil;&atilde;o.");
      (MessageCode == 100050) -> return_util:get_validation_result(100050,
                                   lists:concat([
												 "Obrigat&oacute;rio informar corretamente os campos ",
												 "<strong>Identifica&ccedil;&atilde;o</strong>, <strong>Imagem</strong>",
												 " e <strong>Skin Default</strong>."
												]));
      (MessageCode == 100051) -> return_util:get_validation_result(100051,
								   "Falha ao criar <strong>tema</strong>.");
      (MessageCode == 100052) -> return_util:get_validation_result(100052,
								   "Falha ao alterar <strong>tema</strong>.");
      (MessageCode == 100053) -> return_util:get_validation_result(100053,
                                   lists:concat([
												 "J&aacute; existe outro <strong>tema</strong> ativo para a aplica&ccedil;&atilde;o. ",
												 "N&atilde;o &eacute; permitido ter mais de um tema ativo ao mesmo tempo."
												]));
      (MessageCode == 100054) -> return_util:get_validation_result(100054,
                                   lists:concat([
												 "Obrigat&oacute;rio informar corretamente os campos ",
												 "<strong>Identifica&ccedil;&atilde;o</strong>, ",
												 "<strong>Descri&ccedil;&atilde;o</strong> e <strong>Url Site</strong>."
												]));
      (MessageCode == 100055) -> return_util:get_validation_result(100055,
                                   lists:concat([
												 "J&aacute; existe outra <strong>configura&ccedil;&atilde;o</strong> ativa para a ",
												 " aplica&ccedil;&atilde;o. N&atilde;o &eacute; permitido ter mais de uma configura&ccedil;&atilde;o ativa ",
												 " ao mesmo tempo."
												]));
      (MessageCode == 100056) -> return_util:get_validation_result(100056,
                                   "Falha ao criar <strong>configura&ccedil;&atilde;o</strong>.");
      (MessageCode == 100057) -> return_util:get_validation_result(100057,
                                   "Falha ao alterar <strong>configura&ccedil;&atilde;o</strong>.");
      (MessageCode == 100065) -> return_util:get_validation_result(100065,
                                   "<strong>Pessoa/Usu&aacute;rio</strong> informada n&atilde;o existe.");
      (MessageCode == 100075) -> return_util:get_validation_result(100075,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o ",
												 "pode ser exclu&iacute;da, pois tem <strong>pessoa/usu&aacute;rio</strong> ",
												 "em sua responsabilidade."
												]));
      (MessageCode == 100078) -> return_util:get_validation_result(100078,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, pois tem ",
												 "<strong>pessoa/usu&aacute;rio</strong> em sua responsabilidade."
												]));
      (MessageCode == 100082) -> return_util:get_validation_result(100082,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>e-mail</strong> em sua responsabilidade."
												]));                 
      (MessageCode == 100083) -> return_util:get_validation_result(100083,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o pode ",
												 "ser exclu&iacute;da, pois tem <strong>e-mail</strong> ",
												 "em sua responsabilidade."
												]));
      (MessageCode == 100084) -> return_util:get_validation_result(100084,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>arquivo item menu</strong> ",
												 "em sua responsabilidade."
												]));                
      (MessageCode == 100085) -> return_util:get_validation_result(100085,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o pode ser ",
												 "exclu&iacute;da, pois tem <strong>arquivo item menu</strong> ",
												 "em sua responsabilidade. "
												]));
      (MessageCode == 100086) -> return_util:get_validation_result(100086,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>item menu</strong> em sua responsabilidade."
												]));                         
      (MessageCode == 100087) -> return_util:get_validation_result(100087,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o pode ser ",
												 "exclu&iacute;da, pois tem <strong>item menu</strong> ",
												 "em sua responsabilidade."
												]));
      (MessageCode == 100088) -> return_util:get_validation_result(100088,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>menu</strong> em sua responsabilidade."
												]));            
      (MessageCode == 100089) -> return_util:get_validation_result(100089,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o ",
												 "pode ser exclu&iacute;da, pois tem <strong>menu</strong> ",
												 "em sua responsabilidade."
												]));
      (MessageCode == 100090) -> return_util:get_validation_result(100090,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>m&oacute;dulo</strong> em sua responsabilidade."
												]));          
      (MessageCode == 100091) -> return_util:get_validation_result(100091,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o pode ",
												 "ser exclu&iacute;da, pois tem <strong>m&oacute;dulo</strong> ",
												 "em sua responsabilidade."
												]));
      (MessageCode == 100096) -> return_util:get_validation_result(100096,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>configura&ccedil;&atilde;o e-mail</strong> ",
												 "em sua responsabilidade."
												]));            
      (MessageCode == 100097) -> return_util:get_validation_result(100097,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o ",
												 "n&atilde;o pode ser exclu&iacute;da, pois tem ",
												 "<strong>configura&ccedil;&atilde;o e-mail</strong> em sua responsabilidade."
												]));
      (MessageCode == 100098) -> return_util:get_validation_result(100098,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>imagem</strong> em sua responsabilidade."
												]));            
      (MessageCode == 100099) -> return_util:get_validation_result(100099,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o ",
												 "pode ser exclu&iacute;da, pois tem <strong>imagem</strong> ",
												 "em sua responsabilidade."
												]));
      (MessageCode == 100100) -> return_util:get_validation_result(100100,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>arquivo</strong> em sua responsabilidade."
												]));
      (MessageCode == 100101) -> return_util:get_validation_result(100101,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o pode ",
												 "ser exclu&iacute;da, pois tem <strong>arquivo</strong> ",
												 "em sua responsabilidade."
												]));
      (MessageCode == 100102) -> return_util:get_validation_result(100102,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>tema aplica&ccedil;&atilde;o</strong> em sua responsabilidade."
												]));                 
      (MessageCode == 100103) -> return_util:get_validation_result(100103,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o ",
												 "pode ser exclu&iacute;da, pois tem <strong>tema aplica&ccedil;&atilde;o</strong> ",
												 "em sua responsabilidade."
												]));
      (MessageCode == 100106) -> return_util:get_validation_result(100106,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>configura&ccedil;&atilde;o aplica&ccedil;&atilde;o</strong> ",
												 "em sua responsabilidade."
												]));                     
      (MessageCode == 100107) -> return_util:get_validation_result(100107,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o pode ",
												 "ser exclu&iacute;da, pois tem <strong>configura&ccedil;&atilde;o aplica&ccedil;&atilde;o</strong>",
												 "em sua responsabilidade."
												]));
      (MessageCode == 100112) -> return_util:get_validation_result(100112,
                                   lists:concat([
												 "<strong>Menu</strong> em quest&atilde;o n&atilde;o pode ser exclu&iacute;do, ",
												 "pois tem <strong>item menu</strong> vinculado."
												]));
      (MessageCode == 100113) -> return_util:get_validation_result(100113,
                                   lists:concat([
												 "<strong>Item Menu</strong> em quest&atilde;o n&atilde;o pode ser exclu&iacute;do, ",
												 "pois tem <strong>arquivo item menu</strong> vinculado."
												]));
      (MessageCode == 100114) -> return_util:get_validation_result(100114,
                                   lists:concat([
												 "<strong>Arquivo</strong> em quest&atilde;o n&atilde;o pode ser exclu&iacute;do, ",
												 "pois est&aacute; vinculado &agrave; <strong>item menu</strong>."
												]));
      (MessageCode == 100116) -> return_util:get_validation_result(100116,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o n&atilde;o pode ",
												 "ser exclu&iacute;da, pois tem ",
												 "<strong>configura&ccedil;&atilde;o email</strong> vinculada."
												]));
      (MessageCode == 100127) -> return_util:get_validation_result(100127,
                                   lists:concat([
												 "<strong>Pessoa/Usu&aacute;rio</strong> em quest&atilde;o ",
												 "n&atilde;o pode ser exclu&iacute;da, pois tem ",
												 "<strong>configura&ccedil;&atilde;o AWS S3</strong> em sua responsabilidade."
												]));
      (MessageCode == 100128) -> return_util:get_validation_result(100128,
                                   lists:concat([
												 "Mudan&ccedil;a de categoria n&atilde;o permitida para a ",
												 "<strong>pessoa/usu&aacute;rio</strong> em quest&atilde;o, ",
												 "pois tem <strong>configura&ccedil;&atilde;o AWS S3</strong> ",
												 "em sua responsabilidade."
												]));
      (MessageCode == 100129) -> return_util:get_validation_result(100129,
                                   lists:concat([
												 "Obrigat&oacute;rio informar corretamente os campos ",
												 "<strong>Bucket Name</strong>, <strong>Bucket URL</strong>, ",
												 "<strong>Region</strong>, <strong>Version</strong>, ",
												 "<strong>Key</strong> e <strong>Secret</strong>."
												]));
      (MessageCode == 100130) -> return_util:get_validation_result(100130,
                                   "Falha ao criar <strong>configura&ccedil;&atilde;o AWS S3</strong>.");
      (MessageCode == 100131) -> return_util:get_validation_result(100131,
                                   "Falha ao alterar <strong>configura&ccedil;&atilde;o AWS S3</strong>.");
      (MessageCode == 100132) -> return_util:get_validation_result(100132,
                                   lists:concat([
												 "J&aacute; existe outra <strong>configura&ccedil;&atilde;o AWS S3</strong> ",
												 "ativa para a aplica&ccedil;&atilde;o. N&atilde;o &eacute; permitido ter mais de ",
												 "uma configura&ccedil;&atilde;o AWS S3 ativa ao mesmo tempo."
												]));
      (MessageCode == 100133) -> return_util:get_validation_result(100133,
                                   "Erro ao ativar <strong>configura&ccedil;&atilde;o AWS S3</strong> para a aplica&ccedil;&atilde;o.");
      (MessageCode == 100147) -> return_util:get_validation_result(100147,
                                   lists:concat([
												 "Admin Master n&atilde;o pode criar usu&aacute;rio com permiss&atilde;o ",
												 "<strong>Comunica&ccedil;&atilde;o/Acesso Externa</strong>."
												]));
      (MessageCode == 100148) -> return_util:get_validation_result(100148,
                                   "Falha ao criar <strong>Object</strong>.");
      (MessageCode == 100149) -> return_util:get_validation_result(100149,
                                   "Falha ao alterar <strong>Object</strong>.");
      (MessageCode == 100150) -> return_util:get_validation_result(100150,
                                   lists:concat([
												 "Obrigat&oacute;rio informar corretamente os campos ",
												 "<strong>Identifica&ccedil;&atilde;o</strong> e <strong>Label</strong>."
												]));
      (MessageCode == 100151) -> return_util:get_validation_result(100151,
                                   lists:concat([
												 "<strong>Object</strong> em quest&atilde;o n&atilde;o pode ser exclu&iacute;do, ",
												 "pois tem <strong>object field</strong> vinculado."
												]));
      (MessageCode == 100152) -> return_util:get_validation_result(100152,
                                   "Falha ao criar <strong>Object Field</strong>.");
      (MessageCode == 100153) -> return_util:get_validation_result(100153,
                                   "Falha ao alterar <strong>Object Field</strong>.");
      (MessageCode == 100154) -> return_util:get_validation_result(100154,
                                   lists:concat([
												 "Obrigat&oacute;rio informar corretamente os campos ",
												 "<strong>Identifica&ccedil;&atilde;o</strong>, <strong>Label</strong>, ",
												 "<strong>Tipo</strong> e <strong>Tamanho</strong>."
												]));
      (MessageCode == 100155) -> return_util:get_validation_result(100155,
                                   "<strong>Object</strong> informado n&atilde;o existe.");
      (MessageCode == 100156) -> return_util:get_validation_result(100156,
                                   "Erro ao gerar <strong>C&oacute;digo</strong> para object."); 
      (MessageCode == 100157) -> return_util:get_validation_result(100157,
                                   "Erro ao remover <strong>C&oacute;digo</strong> de object.");
      (MessageCode == 100158) -> return_util:get_validation_result(100158,
                                   "Falha ao criar <strong>Project</strong>.");
      (MessageCode == 100159) -> return_util:get_validation_result(100159,
                                   "Falha ao alterar <strong>Project</strong>.");
      (MessageCode == 100160) -> return_util:get_validation_result(100160,
                                   lists:concat([
												 "<strong>ObjectField</strong> com uma identifica&ccedil;&atilde;o ",
												 "semelhante j&aacute; existe para o Object em quest&atilde;o."
												]));
      (MessageCode == 100161) -> return_util:get_validation_result(100161,
                                   lists:concat([
												 "<strong>Object</strong> com uma identifica&ccedil;&atilde;o semelhante ",
												 "<strong>j&aacute; existe</strong>."
												]));
      (MessageCode == 100162) -> return_util:get_validation_result(100162,
                                   lists:concat([
												 "<strong>Project</strong> com uma identifica&ccedil;&atilde;o ",
												 "semelhante <strong>j&aacute; existe</strong>."
												]));
      (MessageCode == 100163) -> return_util:get_validation_result(100163,
                                   lists:concat([
												 "<strong>Object</strong> em quest&atilde;o n&atilde;o pode ser exclu&iacute;do, ",
												 "pois est&aacute; vinculado em um ou mais <strong>Project</strong>."
												]));
      (MessageCode == 100164) -> return_util:get_validation_result(100164,
                                   "<strong>Identifica&ccedil;&atilde;o</strong> menor que tamanho m&iacute;nimo (5) caracteres.");
      (MessageCode == 100165) -> return_util:get_validation_result(100165,
                                   lists:concat([
												 "<strong>Diret&oacute;rio</strong> j&aacute; existe. Escolha outro ",
												 "nome de projeto, ou exclua o diret&oacute;rio existente."
												]));
      (MessageCode == 100166) -> return_util:get_validation_result(100166,
                                   "Erro ao gerar <strong>C&oacute;digo</strong> para project.");
      (MessageCode == 100167) -> return_util:get_validation_result(100167,
                                   "Erro ao remover <strong>C&oacute;digo</strong> de project."); 
      (MessageCode == 100168) -> return_util:get_validation_result(100168,
                                   "Erro ao atualizar <strong>C&oacute;digo</strong> de project."); 
      (MessageCode == 100232) -> return_util:get_validation_result(100232,
                                   "Falha ao criar <strong>Integra&ccedil;&atilde;o Banco Ita&uacute;</strong>.");
      (MessageCode == 100233) -> return_util:get_validation_result(100233,
                                   "Falha ao alterar <strong>Integra&ccedil;&atilde;o Banco Ita&uacute;</strong>.");
      (MessageCode == 100234) -> return_util:get_validation_result(100234,
                                   "<strong>Integra&ccedil;&atilde;o Banco Ita&uacute;</strong> informada n&atilde;o existe.");
      (MessageCode == 100270) -> return_util:get_validation_result(100270,
                                   lists:concat([
												 "Limite de utiliza&ccedil;&atilde;o do recurso foi extrapolado. ",
												 "Aguarde um minuto e tente novamente."
												]));
      (MessageCode == 100271) -> return_util:get_validation_result(100271,
                                   lists:concat([
												 "Falha ao tentar enviar mensagem. ",
												 "Por favor aguarde um momento e tente novamente."
												]));
      (MessageCode == 100272) -> return_util:get_validation_result(100272,
                                   "Falha ao gerar <strong>Boleto Ita&uacute;</strong>.");
      (MessageCode == 100273) -> return_util:get_validation_result(100273,
                                   "Falha ao alterar <strong>Boleto Ita&uacute;</strong>.");  
      (MessageCode == 100275) -> return_util:get_validation_result(100275,
                                   lists:concat([
												 "<strong>N&uacute;mero de Pedido</strong> informado j&aacute; existe, ",
												 "para a respectiva configura&ccedil;&atilde;o de boleto Ita&uacute;."
												]));
      (MessageCode == 100278) -> return_util:get_validation_result(100278,
                                   lists:concat([
												 "<strong>Integra&ccedil;&atilde;o Banco Ita&uacute;</strong> em quest&atilde;o ",
												 "n&atilde;o pode ser exclu&iacute;da, pois est&aacute; vinculada ",
												 "&agrave; <strong>boleto ita&uacute;</strong>."
												]));
	  (MessageCode == 100279) -> return_util:get_validation_result(100279,
                                   "Falha ao criar <strong>Wallet</strong>.");
      (MessageCode == 100280) -> return_util:get_validation_result(100280,
                                   "Falha ao alterar <strong>Wallet</strong>.");
	  (MessageCode == 100281) -> return_util:get_validation_result(100281,
                                   "Falha ao alterar senha <strong>Wallet</strong>.");  
	  (MessageCode == 100282) -> return_util:get_validation_result(100282,
                                   "Falha ao recuperar senha <strong>Wallet</strong>. Verifique sua frase de recupera&ccedil;&atilde;o.");
	  (MessageCode == 100283) -> return_util:get_validation_result(201,
                                   "Senha <strong>Wallet</strong> alterada com sucesso.");
	  (MessageCode == 100284) -> return_util:get_validation_result(201,
                                   "Senha <strong>Wallet</strong> recuperada com sucesso.");
	  (MessageCode == 100285) -> return_util:get_validation_result(100285,
                                   "Falha ao criar <strong>Transa&ccedil;&atilde;o</strong>.");
      (MessageCode == 100286) -> return_util:get_validation_result(100286,
                                   "Falha ao alterar <strong>Transa&ccedil;&atilde;o</strong>.");
	  (MessageCode == 100287) -> return_util:get_validation_result(100287,
                                   "<strong>Wallet</strong> informada n&atilde;o existe.");
	  (MessageCode == 100288) -> return_util:get_validation_result(100288,
                                   "Falha ao criar <strong>Master Node</strong>.");
      (MessageCode == 100289) -> return_util:get_validation_result(100289,
                                   "Falha ao alterar <strong>Master Node</strong>.");
	  (MessageCode == 100290) -> return_util:get_validation_result(100290,
                                   "J&aacute; existe um <strong>Master Node</strong> para a <strong>Wallet</strong>.");
      true -> system_message(0)
    end.





