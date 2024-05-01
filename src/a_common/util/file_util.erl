-module(file_util).

-export(
   		[
		 get_random_temp_filename/1,
		 valid_extensions/0,
		 valid_upload_extensions/0,
		 get_extension/1, 
		 get_mime_type_file_by_name/1,
		 get_mime_by_content/2,
		 read_content/2,
		 read_file/1,
		 drop_file/1,
		 write_file/2
		]
	   ).

get_random_temp_filename(Extension) ->
	FileName = filename:join(["/tmp", erlang:unique_integer()]),
	case lists:member(Extension, valid_extensions()) of
		true ->
			lists:concat([FileName, ".", Extension]);
		_ ->
			lists:concat([FileName, ".txt"])
	end.

valid_extensions() ->
    [
	 <<"sql">>, <<"txt">>, <<"html">>, <<"pdf">>, <<"xml">>,
	 <<"webmanifest">>, <<"ts">>, <<"js">>, <<"sh">>, <<"json">>
	].

valid_upload_extensions() ->
    [<<"jpeg">>, <<"jpg">>, <<"png">>, <<"gif">>, <<"bmp">>, 
	 <<"pdf">>, <<"doc">>, <<"docx">>, <<"xls">>, <<"xlsx">>, <<"ppt">>, <<"pptx">>].

get_extension(FileName) ->
	case (datatransform_util:is_undefined(FileName)) of
		true ->
			false;
		_ ->
			Parts = string:split(FileName, "."),
			case (length(Parts) < 2) of
				true ->
					undefined;
				_ ->
					string:lowercase(lists:nth(length(Parts), Parts))
			end
	end.

get_mime_type_file_by_name(FileBase64) ->
	case (datatransform_util:is_undefined(FileBase64)) of
		true ->
			undefined;
		_ ->
			List0 = string:split(FileBase64, ";"),
			case (length(List0) < 2) of
				true ->
					undefined;
				_ ->
					List1 = string:split(lists:nth(1, List0), ":"),
					case (length(List1) < 2) of
						true ->
							undefined;
						_ ->
							string_util:trim(lists:nth(2, List1))
					end
			end
	end.

get_mime_by_content(MimeType, FileBase64) ->
	Header = string:lowercase(read_header(FileBase64)),
	IsDoc = (MimeType == <<"application/vnd.openxmlformats-officedocument.wordprocessingml.document">>),
	IsSheet = (MimeType == <<"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet">>),
	IsPresentation = (MimeType == <<"application/vnd.openxmlformats-officedocument.presentationml.presentation">>),
	NewMsFormat = lists:member(Header, [<<"504b0304">>, <<"504b0506">>, <<"504b0708">>]),
	Header424d = string:find(Header, <<"424d">>),
    if
		(Header == Header424d) -> <<"image/bmp">>;
		(Header == <<"89504e47">>) -> <<"image/png">>;
		(Header == <<"47494638">>) -> <<"image/gif">>;
		(Header == <<"ffd8ffe0">>) -> <<"image/jpeg">>;
		(Header == <<"ffd8ffe1">>) -> <<"image/jpeg">>;
		(Header == <<"ffd8ffe2">>) -> <<"image/jpeg">>;
		(Header == <<"ffd8ffe3">>) -> <<"image/jpeg">>;
		(Header == <<"ffd8ffe8">>) -> <<"image/jpeg">>;
        (Header == <<"25504446">>) -> <<"application/pdf">>;
		((Header == <<"d0cf11e0">>) and (MimeType == <<"application/vnd.ms-excel">>)) -> MimeType;
        ((Header == <<"d0cf11e0">>) and (MimeType == <<"application/msword">>)) -> MimeType;
        ((Header == <<"d0cf11e0">>) and (MimeType == <<"application/vnd.ms-powerpoint">>)) -> MimeType;
		(NewMsFormat and IsDoc) -> MimeType;
		(NewMsFormat and IsSheet) -> MimeType;
        (NewMsFormat and IsPresentation) -> MimeType;
		true -> undefined
	end.

read_content(FileBase64, Decode) ->
	case (datatransform_util:is_undefined(FileBase64)) of
		true ->
			undefined;
		_ ->
			case Decode of
				true -> 
					base64:decode(lists:nth(2, string:split(FileBase64, ",")));
				_ ->
					lists:nth(2, string:split(FileBase64, ","))
			end
	end.

read_file(Filename) ->
	Result = tuple_to_list(file:read_file(Filename)),
	case lists:nth(1, Result) of
		ok ->
			lists:nth(2, Result);
		_ ->
			undefined
	end.

drop_file(FileName) ->
	Result = file:delete(FileName),
	case Result of
		ok ->
			true;
		_ ->
			false
	end.

write_file(Filename, ContentBinary) ->
	Result = file:write_file(Filename, ContentBinary),
	case Result of
		ok ->
			true;
		_ ->
			false
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_header(FileBase64) ->
	case (datatransform_util:is_undefined(FileBase64)) of
		true ->
			undefined;
		_ ->
			ArrayBuffer = base64:decode(lists:nth(1, string:split(FileBase64, ","))),
		    lists:concat([
						  integer_to_list(lists:nth(1, ArrayBuffer), 16),
						  integer_to_list(lists:nth(2, ArrayBuffer), 16),
						  integer_to_list(lists:nth(3, ArrayBuffer), 16),
						  integer_to_list(lists:nth(4, ArrayBuffer), 16)
						  ])
	end.
