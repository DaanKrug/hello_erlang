-module(s3_file_handler).

-export([validate_and_upload_s3/3, delete_from_s3/2]).

validate_and_upload_s3(FileName, FileBase64, OwnerId) ->
	case (validator:validate_mime(FileName, FileBase64)) of
		ok ->
			upload_to_s3(FileName, FileBase64, OwnerId);
		_ ->
			undefined
	end.
  
delete_from_s3(Link, OwnerId) ->
	case string:find(Link, ".amazonaws.com") of
		nomatch ->
			true;
		_ ->
			drop_from_s3(Link, OwnerId)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
upload_to_s3(FileName, FileBase64, OwnerId) ->
	AppConfig = appconfig_dao:get_appconfig(),
	S3Config = s3config_dao:get_s3Config(),
	Undefined1 = datatransform_util:is_undefined(AppConfig),
	Undefined2 = datatransform_util:is_undefined(S3Config),
	case (Undefined1 or Undefined2) of
		true ->
			undefined;
		_ ->
			AppId = number_util:to_integer(proplists:get_value(<<"id">>, AppConfig, 0)),
			BucketName = string_util:trim(proplists:get_value(<<"bucketName">>, S3Config, "")),
			BucketUrl = string_util:trim(proplists:get_value(<<"bucketUrl">>, S3Config, "")),
			case ((AppId == 0) or (BucketName == "") or (BucketUrl == "")) of
				true ->
					undefined;
				_ ->
					FileDir = lists:concat([AppId, "_", OwnerId, "/", FileName]),
					put_to_s3_bucket(BucketName, BucketUrl, FileDir, FileBase64)
			end
	end.

put_to_s3_bucket(BucketName, BucketUrl, FileDir, FileBase64) ->
	ContentBinary = file_util:read_content(FileBase64, true),
	case datatransform_util:is_undefined(ContentBinary) of
		true ->
			undefined;
		_ ->
			Result = erlcloud_s3:put_object(BucketName, FileDir, ContentBinary),
			case (datatransform_util:is_undefined(Result) or (Result == false)) of
				true ->
					undefined;
				_ ->
					lists:concat([BucketUrl, "/", FileDir])
			end
	end.


drop_from_s3(Link, OwnerId) ->
	AppConfig = appconfig_dao:get_appconfig(),
	S3Config = s3config_dao:get_s3Config(),
	Undefined1 = datatransform_util:is_undefined(AppConfig),
	Undefined2 = datatransform_util:is_undefined(S3Config),
	case (Undefined1 or Undefined2) of
		true ->
			false;
		_ ->
			AppId = number_util:to_integer(proplists:get_value(<<"id">>, AppConfig, 0)),
			BucketName = string_util:trim(proplists:get_value(<<"bucketName">>, S3Config, "")),
			BucketUrl = string_util:trim(proplists:get_value(<<"bucketUrl">>, S3Config, "")),
			case ((AppId == 0) or (BucketName == "") or (BucketUrl == "")) of
				true ->
					false;
				_ ->
					LinkParts = string:split(Link, "/"),
					FileDir = lists:concat([AppId, "_", OwnerId, "/", 
											lists:nth(length(LinkParts), LinkParts)]),
					Result = erlcloud_s3:delete_object(BucketName, FileDir),
					case (datatransform_util:is_undefined(Result) or (Result == false)) of
						true ->
							false;
						_ ->
							true
					end
			end
	end.


	
