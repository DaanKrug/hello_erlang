-module(sanitizer_util).

-export([
		 sanitize_email/1, 
		 sanitize_url/1, 
		 sanitize_web_url/1,
		 sanitize/1, 
		 sanitize_all/4, 
		 sanitize_filename/2
		]).

sanitize_email(Email) ->
	Email2 = string_util:trim(string:lowercase(Email)),
	Email3 = sanitize_all(Email2, false, 100, <<"email">>),
	Valid = validator:email(Email3),
	MinLengthOk = validator:min_length(Email3,5),
	MaxLengthOk = validator:max_length(Email3,100),
	case ((Valid == ok) and (MinLengthOk == ok) and (MaxLengthOk == ok)) of
		true ->
			Email3;
		_ ->
			""
	end.

sanitize_url(Url) ->
	sanitize_all(Url, false, 0, <<"url">>).

sanitize_web_url(Url) ->
	Valid = validator:url(sanitize_url(Url)),
	case (Valid == ok) of
		true -> 
			Url;
		_ ->
			""
	end.

sanitize(Input) ->
	Bin = string:replace(struct_util:normalize_list_for_string(Input), "quirbula", ",", all),
	Bin2 = string:replace(struct_util:normalize_list_for_string(Bin), "xcrept ", "select ", all),
	Bin3 = string:replace(struct_util:normalize_list_for_string(Bin2), "xoo ", "and ", all),
	Bin4 = string:replace(struct_util:normalize_list_for_string(Bin3), "yoo ", "or ", all),
	Bin5 = string:replace(struct_util:normalize_list_for_string(Bin4), "x43re ", "where ", all),
	Bin6 = string:replace(struct_util:normalize_list_for_string(Bin5), "despint", "distinct", all),
	Bin7 = string:replace(struct_util:normalize_list_for_string(Bin6), "xstrike ", "like ", all),
	Bin8 = string:replace(struct_util:normalize_list_for_string(Bin7), "quaspa", "'", all),
	Bin9 = struct_util:normalize_list_for_string(Bin8),
	verify_forbidden(Bin9).

sanitize_all(Input, IsNumeric, MaxLength, ValidChars) ->
	case ((is_float(Input) or is_integer(Input)) and IsNumeric) of
		true ->
			Input;
		_ ->
		    Bin = verify_forbidden(string_util:trim(Input)),
			case (Bin /= string_util:trim(Input)) of
				true -> 
					get_empty_value(IsNumeric);
				_ ->
					MaxLength2 = number_util:coalesce(MaxLength, 0, false),
					sanitize_input(Bin, IsNumeric, MaxLength2, ValidChars)
			end
	end.

sanitize_filename(Name, MaxLength) ->
	MaxLength2 = number_util:coalesce(MaxLength, 10, true),
    Name2 = sanitize_input(Name, false, MaxLength2, <<"filename">>),
	case (string_util:trim(Name2) == "")  of
		true -> 
			string_util:generate_random_filename(MaxLength2);
		_ ->
			Name2
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
verify_forbidden(Input) ->
    Valid = validator:forbidden(Input),
	case (Valid == ok) of
		true -> 
			Input;
		_ ->
			""
	end.

get_empty_value(IsNumeric) ->
	case (IsNumeric == true) of
		true -> 
			"0";
		_ -> 
			""
	end.

get_empty_chars(IsNumeric) ->
	case IsNumeric of
		true -> 
			char_util:nums();
		_ -> 
			char_util:alpha_nums()
	end.

sanitize_input(Bin, IsNumeric, MaxLength, ValidChars) ->
    Bin2 = string_util:trim(Bin),
	case (Bin2 == "") of
		true ->
			get_empty_value(IsNumeric);
		_ ->
			EnabledChars = get_valid_chars_for_sanitize_input(ValidChars, IsNumeric),
			Translated = datatransform_util:translate_and_validate(Bin2, EnabledChars, IsNumeric),
			verify_translated(Translated, MaxLength, IsNumeric)
	end.

verify_translated(Translated, MaxLength, IsNumeric) ->
	Length = string:length(Translated),
	case (((MaxLength > 0) and (Length > MaxLength)) or (Length == 0)) of
		true ->
			get_empty_value(IsNumeric);
		_ ->
			case (IsNumeric) of
				true -> 
					verify_invalid_translated_number(Translated);
				_ -> 
					Translated
			end
	end.

verify_invalid_translated_number(Translated) ->
	[Header | Tail] = string:split(Translated, ""),
	case ((Header == ".") or (lists:member("-", Tail))) of
		false ->
			Translated;
		_ ->
			"0"
	end.

get_valid_chars_for_sanitize_input(ValidChars, IsNumeric) ->
	case ValidChars of
		null ->
			get_empty_chars(IsNumeric);
		undefined ->
			get_empty_chars(IsNumeric);
		<<"A-z0-9">> -> 
			char_util:alpha_nums();
		<<"A-z0-9Name">> -> 
			char_util:alpha_nums_name();
		<<"A-z0-9|">> -> 
			char_util:alpha_nums_pipe();
		<<"0-9">> -> 
			char_util:nums();
		<<"0-9OnlyNums">> -> 
			char_util:only_nums();
		<<"A-z">> -> 
			char_util:alphas();
		<<"a-z">> -> 
			char_util:alpha_lowers();
		<<"A-Z">> -> 
			char_util:alpha_uppers();
		<<"DATE_SLASH">> -> 
			char_util:date_slash();
		<<"DATE_SQL">> -> 
			char_util:date_sql();
		<<"email">> -> 
			char_util:email_chars();
		<<"password">> -> 
			char_util:password_chars();
		<<"url">> -> 
			char_util:url_chars();
		<<"hex">> -> 
			char_util:hex_chars();
		<<"filename">> -> 
			char_util:filename_chars();
		<<"money">> -> 
			char_util:money_chars();
		_ -> 
			split_valid_chars_for_sanitize_input(ValidChars, IsNumeric)
	end.

split_valid_chars_for_sanitize_input(ValidChars, IsNumeric) ->
	ValidChars2 = string_util:trim(ValidChars),
	ValidChars3 = string:replace(ValidChars2, " ", "", all),
	ValidChars4 = string:split(ValidChars3, ","),
	case (length(ValidChars4) > 0) of
		true -> 
			ValidChars4;
		_ -> 
			get_empty_chars(IsNumeric)
	end.
