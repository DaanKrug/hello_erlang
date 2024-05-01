-module(validator).

-export(
   		[
		 min_length/2, 
		 max_length/2, 
		 email/1, 
		 url/1, 
		 forbidden/1, 
		 validate_key/2,
		 validate_mime/2
		]
	   ).

validate_key(Key, Rkey) ->
	if
		(Key == "") -> error;
		(Rkey == "") -> error;
		true ->
			Reverted = cipherform_transmutter:decipher(Key),
		    case (string_util:equals(Reverted, Rkey)) of
				true ->
					ok;
				_ ->
					error
			end
	end.

email(Email) ->
	regexp_match("^[-\\w.]+@([A-z0-9][-A-z0-9]+\\.)+[A-z]{2,}$", Email, <<"Invalid Email">>).

url(Url) ->
	case (is_web_url_prefix(Url) == false) of
		true -> 
			{error, io_lib:format("Invalid URL: ~b", [Url])};
		_ -> 
			ok
	end.

forbidden(Input) ->
	case struct_util:contained_in_one_of_list(Input, char_util:forbidden()) of
		true -> 
			{error, io_lib:format("Invalid Input: ~b", [Input])};
		_ -> 
			ok
	end.
		
min_length(MinLength, Bin) when byte_size(Bin) < MinLength ->
    {error, io_lib:format("Min length: ~b", [MinLength])};
min_length(_, _) ->
    ok.

max_length(MaxLength, Bin) when byte_size(Bin) > MaxLength ->
    {error, io_lib:format("Max length: ~b", [MaxLength])};
max_length(_, _) ->
    ok.

validate_mime(FileName, FileBase64) ->
	Extension = file_util:get_extension(FileName),
	MimeType = file_util:get_mime_type_file_by_name(FileBase64),
	Undefined = datatransform_util:is_emptystring(Extension)
                or datatransform_util:is_emptystring(MimeType),
	case Undefined of
		true ->
			error;
		_ ->
			case (MimeType == file_util:get_mime_by_content(MimeType, FileBase64)) of
				true -> 
					ok;
				_ ->
					error
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
regexp_match(Re, Bin, ErrorMsg) ->
    case re:run(Bin, Re) of
        {match, _} ->
            ok;
        nomatch ->
            {error, ErrorMsg}
    end.

is_web_url_prefix(Url) ->
	Url2 = string:find(Url, "http://"),
	case Url2 of
		nomatch ->
			Url3 = string:find(Url, "https://"),
			case Url3 of
				nomatch ->
					false;
				_ ->
					(Url3 == Url)
			end;
		_ ->
			(Url2 == Url)
	end.


