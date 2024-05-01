-module(itaubankconfig_util).

-export([get_search_data/5, get_bank_billet_data/19]).

-define(KEY_SIZE, 16).
-define(SHOP_KEY_SIZE, 26).

get_search_data(A1Itaukey, A2Shoplinecode, OrderNumber, Format, A3Shoplinekey) ->
	Validation = validate_search_data(A2Shoplinecode, OrderNumber, Format, A3Shoplinekey),
	case datatransform_util:is_undefined(Validation) of
		true ->
			get_search_data2(A1Itaukey, A2Shoplinecode, OrderNumber, Format, A3Shoplinekey);
		_ ->
			Validation
	end.

get_bank_billet_data(A1Itaukey, A2Shoplinecode, OrderNumber, OrderValue0, Observation, 
					 A3Shoplinekey, OwnerName, CpfInscription, CpfNumber, OwnerAddress, 
					 OwnerNeighborhood, OwnerCep, OwnerCity, OwnerState, ExpiresDate, UrlRetorno, 
					 AdditionalText1, AdditionalText2, AdditionalText3) ->	
	OrderValue = string:replace(OrderValue0, ".", ",", all),
    Validation = validate_bank_billet_data(A2Shoplinecode, OrderNumber, OrderValue, A3Shoplinekey,
                                           CpfInscription, AdditionalText1, AdditionalText2, AdditionalText3),
    case datatransform_util:is_undefined(Validation) of
		true ->
			prepare_bank_billet_data(A1Itaukey, A2Shoplinecode, OrderNumber, OrderValue, Observation,
                                     A3Shoplinekey, OwnerName, CpfInscription, CpfNumber, OwnerAddress, 
									 OwnerNeighborhood, OwnerCep, OwnerCity, OwnerState, ExpiresDate, 
									 UrlRetorno, AdditionalText1, AdditionalText2, AdditionalText3);
		_ ->
			Validation
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_bank_billet_data(A2Shoplinecode, OrderNumber, OrderValue, A3Shoplinekey,
                          CpfInscription, AdditionalText1, AdditionalText2, AdditionalText3) ->
	Validation = validate_order_value(OrderValue),
	case datatransform_util:is_undefined(Validation) of
		true ->
			Validation2 = validate_search_data(A2Shoplinecode, OrderNumber, "0", A3Shoplinekey),
			case datatransform_util:is_undefined(Validation2) of
				true ->
					ValidCpfInscription = lists:member(CpfInscription, ["  ","01","02"]),
					InvalidAdditionalText1 = string:length(AdditionalText1) > 60,
					InvalidAdditionalText2 = string:length(AdditionalText2) > 60,
					InvalidAdditionalText3 = string:length(AdditionalText3) > 60,
					if
						(ValidCpfInscription == false) ->
							"Erro: tipo pessoa inválido.";
						(InvalidAdditionalText1) ->
							"Erro: observação adicional 1 inválida.";
						(InvalidAdditionalText2) ->
							"Erro: observação adicional 2 inválida.";
						(InvalidAdditionalText3) ->
							"Erro: observação adicional 3 inválida.";
						true ->
							undefined
					end;
				_ ->
					Validation2
			end;
		_ ->
			Validation
	end.

validate_order_value(OrderValue) ->
	Value = number_util:to_float(OrderValue),
	HasComma = string:find(OrderValue, ","),
	Splitted = string:split(OrderValue, ","),
	Length1 = length(Splitted),
	Length2 = length(lists:nth(2, Splitted)),
	if
		(OrderValue == "") ->
			"Erro: valor da compra inválido.";
		(Value > 99999999.99) ->	 
        	"Erro: valor da compra deve possuir no máximo 8 posições antes da virgula.";
		(HasComma == nomatch) ->
			undefined;
		(Length1 /= 2) ->
            "Erro: valor da compra não é numérico. Mais de uma vírgula encontrada";
		(Length2 /= 2) ->
            "Erro: valor decimal da compra deve possuir 2 posições após a virgula.";
    	true -> 
			undefined
    end.

validate_search_data(A2Shoplinecode, OrderNumber, Format, A3Shoplinekey) ->
	IsNumber = number_util:to_integer(OrderNumber),
	Length1 = string:length(A2Shoplinecode),
	Length2 = string:length(A3Shoplinekey),
	Length3 = string:length(OrderNumber),
	InvalidFormat = lists:member(Format, ["0","1"]) == false,
	if
		(IsNumber == false) -> 
			"Erro: número do pedido não é numérico.";
		(Length1 /= ?SHOP_KEY_SIZE) ->
			"Erro: tamanho do codigo da empresa diferente de 26 posições.";
		(Length2 /= ?KEY_SIZE) ->
			"Erro: tamanho da chave da chave diferente de 16 posições.";
		(Length3 /= 8) ->
			"Erro: número do pedido inválido.";
		(InvalidFormat) ->
			"Erro: formato inválido.";
		true ->
			undefined
    end.

get_search_data2(A1Itaukey, A2Shoplinecode, OrderNumber, Format, A3Shoplinekey) ->
	Content = itaubankconfig_cipher:cipher_core(lists:concat([OrderNumber, Format]), A3Shoplinekey),
    Content2 = itaubankconfig_cipher:cipher_core(lists:concat([A2Shoplinecode, Content]), A1Itaukey),
	string:uppercase(itaubankconfig_cipher:convert(Content2)).


prepare_bank_billet_data(A1Itaukey, A2Shoplinecode, OrderNumber, OrderValue0, Observation0,
                         A3Shoplinekey, OwnerName0, CpfInscription, CpfNumber0, OwnerAddress0, 
						 OwnerNeighborhood0, OwnerCep0, OwnerCity0, OwnerState0, ExpiresDate0, 
						 UrlRetorno0, AdditionalText10, AdditionalText20, AdditionalText30) ->
	OrderValue = string:replace(OrderValue0, ",", "", all),
	Observation = string_util:right_spaces(sanitizer_util:translate(Observation0), 40),
	OwnerName = string_util:right_spaces(sanitizer_util:translate(OwnerName0), 30),
	CpfNumber = string_util:right_spaces(CpfNumber0, 14),
	OwnerAddress = string_util:right_spaces(sanitizer_util:translate(OwnerAddress0), 40),
	OwnerNeighborhood = string_util:right_spaces(sanitizer_util:translate(OwnerNeighborhood0), 15),
	OwnerCep = string_util:right_spaces(OwnerCep0, 8),
	OwnerCity = string_util:right_spaces(sanitizer_util:translate(OwnerCity0), 15),
	OwnerState = string_util:right_spaces(OwnerState0, 2),
	ExpiresDate = string_util:right_spaces(ExpiresDate0, 8),
	UrlRetorno = string_util:right_spaces(UrlRetorno0, 60),
	AdditionalText1 = string_util:right_spaces(sanitizer_util:translate(AdditionalText10), 60),
	AdditionalText2 = string_util:right_spaces(sanitizer_util:translate(AdditionalText20), 60),
	AdditionalText3 = string_util:right_spaces(sanitizer_util:translate(AdditionalText30), 60),
	Content = lists:concat([
							OrderNumber, OrderValue, Observation, OwnerName, CpfInscription,
							CpfNumber, OwnerAddress, OwnerNeighborhood, OwnerCep, OwnerCity,
							OwnerState, ExpiresDate, UrlRetorno, AdditionalText1,
							AdditionalText2, AdditionalText3
							]),
	Content2 = itaubankconfig_cipher:cipher_core(Content, A3Shoplinekey),
	Content3 = itaubankconfig_cipher:cipher_core(lists:concat([A2Shoplinecode, Content2]), A1Itaukey),
	string:uppercase(itaubankconfig_cipher:convert(Content3)).
