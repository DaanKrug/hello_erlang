-module(s3config_dao).

-behaviour(dao_behaviour).

-export(
   		[
		 get_database_name/0,
		 user_is_owner_s3config/1, 
		 already_has_active/1,
		 load_object_by_id/1, 
		 create_object/8, 
		 update_object/8, 
		 load_all_objects/4, 
		 load_all_objects_for_public/4, 
		 delete_object/1, 
		 restore_object/1, 
		 trully_delete_object/1, 
		 get_column_type/1,
		 get_s3Config/0,
		 bootstrap_s3_config/0
		]
	   ).

get_database_name() ->
	lists:concat([base_dao:get_database_prefix_name(), "_config"]).

user_is_owner_s3config(UserId) ->
  	Sql = "select id from s3config where ownerId = ? limit 1",
	(base_dao:load_count(Sql, [UserId], ?MODULE)) > 0.

already_has_active(Id) ->
    Sql = lists:concat([
						"select id from s3config where ",
						"(deleted_at is null or deleted_at = '0000-00-00 00:00:00')",
						" and id <> ? and active = true"
					   ]),
    (base_dao:load_count(Sql, [Id], ?MODULE)) > 0.

load_object_by_id(Id) ->
	Sql = lists:concat([
						"select id, bucketName, bucketUrl, region, version, keyy, ", 
	                    "secret, active, ownerId from s3config where id = ? limit 1"
					   ]),
	base_dao:load_by_id(Sql, Id, ?MODULE).

create_object(BucketName, BucketUrl, Region, Version, Key, Secret, Active, OwnerId) ->
	CreatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"insert into s3config(bucketName, bucketUrl, region, version, ",
						"keyy, secret, active, ownerId, created_at) values (?,?,?,?,?,?,?,?,?)"
					   ]),
	ParamValues = [
				   BucketName, BucketUrl, Region, Version, Key, Secret, 
				   boolean_util:to_integer(Active), OwnerId, CreatedAt
				  ],
    Result = base_dao:create(Sql, ParamValues, ?MODULE),
	bootstrap_s3_config(),
	Result.

update_object(Id, BucketName, BucketUrl, Region, Version, Key, Secret, Active) ->
	UpdatedAt = date_util:time_to_sql_datetime(0, true),
    Sql = lists:concat([
						"update s3config set bucketName = ?, bucketUrl = ?, region = ?, ",
						"version = ?, keyy = ?, secret = ?, active = ?, updated_at = ? where id = ?"
					   ]),
	ParamValues = [
				   BucketName, BucketUrl, Region, Version, Key, Secret, 
				   boolean_util:to_integer(Active), UpdatedAt, Id
				  ],
    Result = base_dao:update(Sql, ParamValues, ?MODULE),
	bootstrap_s3_config(),
	Result.

load_all_objects(Page, Rows, Conditions, FlagAsDeleted) -> 
	Sql = lists:concat([
						"select id, bucketName, bucketUrl, region, version, keyy, ",
						"secret, active, ownerId from s3config"
					   ]),
	OrderByCondition = " order by id asc ",
	base_dao:load_all(Sql, Page, Rows, Conditions, FlagAsDeleted, OrderByCondition, ?MODULE).

load_all_objects_for_public(_Page, _Rows, _Conditions, _FlagAsDeleted) -> 
	[].

delete_object(Id) -> 
    Sql = "update s3config set deleted_at = ? where id = ?",
	Now = date_util:time_to_sql_datetime(0, true),
	base_dao:update(Sql, [Now, Id], ?MODULE).

restore_object(Id) -> 
	Sql = "update s3config set deleted_at = ? where id = ?",
	base_dao:update(Sql, ["0000-00-00 00:00:00", Id], ?MODULE).

trully_delete_object(Id) -> 
	Sql = "delete from s3config where id = ?",
	base_dao:delete(Sql, [Id], ?MODULE).

get_column_type(ColumnName) ->
	IsNumber = string_util:equals(ColumnName, <<"id">>),
	IsNumber2 = string_util:equals(ColumnName, <<"active">>),
	IsNumber3 = string_util:equals(ColumnName, <<"ownerId">>),
	case (IsNumber or IsNumber2 or IsNumber3) of
		true ->
			"number";
		_ ->
			"binary"
	end.

get_s3Config() ->
	S3Configs = load_active(),
	case (datatransform_util:is_emptylist(S3Configs)) of
		true ->
			undefined;
		_ ->
			lists:nth(1, S3Configs)
	end.

bootstrap_s3_config() ->
	erlcloud_ec2:configure("", "", ""),
	S3Config = get_s3Config(),
	case datatransform_util:is_undefined(S3Config) of
		true ->
			error; 
		_ ->
			AccessKeyId = proplists:get_value(<<"keyy">>, S3Config, ""),
		    SecretAccessKey = proplists:get_value(<<"secret">>, S3Config, ""),
			Region = proplists:get_value(<<"region">>, S3Config, ""),
			HostName = "ec2.amazonaws.com",
			application:set_env(erlcloud, aws_access_key_id, AccessKeyId),
		    application:set_env(erlcloud, aws_secret_access_key, SecretAccessKey),
		    application:set_env(erlcloud, aws_security_token, ""),
		    application:set_env(erlcloud, aws_region, Region),
		    erlcloud_ec2:configure(AccessKeyId, SecretAccessKey, HostName),
			ok
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_active() ->
	Sql = lists:concat([
						"select id, bucketName, bucketUrl, region, version, keyy, ", 
	                    "secret, active, ownerId from s3config where active = true limit 1"
					   ]),
	base_dao:load_by_params(Sql, [], ?MODULE).
