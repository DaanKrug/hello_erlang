-module(s3config_handler).

-behaviour(handler_behaviour).

-export([
		 init/2,
		 object_class_name/0,
		 get_base_url/0,
		 get_max_access_times_on_minute/0,
		 get_categories/0,
		 get_table_name/0,
		 authorize_operation/3,
		 validate_to_save/1,
		 validate_to_update/2,
		 validate_to_delete/1,
		 validate_to_restore/1,
		 validate_to_trully_delete/1,
		 save/3,
		 edit/1,
		 load_all/4,
		 load_all_for_public/4,
		 update/4,
		 delete/3,
		 restore/3,
		 trully_delete/3,
		 need_auth/2,
		 skip_validate_ownership/4,
		 authenticate/3,
		 logoff/2,
		 special_op1/3,
		 special_op2/3
		]).

init(Req, Opts) ->
	rest_handler:handle_request(Req, Opts, ?MODULE).

object_class_name() ->
	<<"Configura&ccedil;&atilde;o AWS S3">>.

get_base_url() -> 
	<<"/s3configs">>.

get_max_access_times_on_minute() ->
	25.

get_categories() ->
	[<<"admin_master">>].

get_table_name() ->
	<<"s3config">>.

authorize_operation(_BodyPropList, _UrlParams, _MethodToCall) ->
	true.

need_auth(_BodyPropList, _MethodToCall) ->
	true.

skip_validate_ownership(_MethodToCall, _OwnerId, _Token, _UrlParams) ->
	false.

validate_to_save(BodyPropList) -> 
	BucketName = s3config_dataextractor:getbucket_name(BodyPropList, ""),
    BucketUrl = s3config_dataextractor:getbucket_url(BodyPropList, ""),
    Region = s3config_dataextractor:get_region(BodyPropList, ""),
    Version = s3config_dataextractor:get_version(BodyPropList, ""),
    Key = s3config_dataextractor:get_key(BodyPropList, ""),
    Secret = s3config_dataextractor:get_secret(BodyPropList, ""),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
    if
        (OwnerId < 1) -> [messages_util:system_message(412)];
	    ((BucketName == "") or (BucketUrl == "")) -> [messages_util:system_message(100129)];
		((Region == "") or (Version == "")) -> [messages_util:system_message(100129)];
		((Key == "") or (Secret == "")) -> [messages_util:system_message(100129)];
        true -> []
    end.

validate_to_update(BodyPropList, Id) -> 
	S3Config = lists:nth(1, edit(Id)),
	BucketName = s3config_dataextractor:getbucket_name(BodyPropList, 
													   proplists:get_value(<<"bucketName">>, S3Config, "")),
    BucketUrl = s3config_dataextractor:getbucket_url(BodyPropList, 
													 proplists:get_value(<<"bucketUrl">>, S3Config, "")),
    Region = s3config_dataextractor:get_region(BodyPropList,
											   proplists:get_value(<<"region">>, S3Config, "")),
    Version = s3config_dataextractor:get_version(BodyPropList, 
												 proplists:get_value(<<"version">>, S3Config, "")),
    Key = s3config_dataextractor:get_key(BodyPropList, 
										 proplists:get_value(<<"keyy">>, S3Config, "")),
    Secret = s3config_dataextractor:get_secret(BodyPropList, 
                                               proplists:get_value(<<"secret">>, S3Config, "")),
	ActiveOld = proplists:get_value(<<"active">>, S3Config, false),
	ActiveNew = generic_dataextractor:get_bool(BodyPropList, <<"active">>, ActiveOld),
	OwnerId = generic_dataextractor:get_owner_id(BodyPropList),
	if
        ((Id < 1) or (OwnerId < 1)) -> 
			[messages_util:system_message(412)];
		((BucketName == "") or (BucketUrl == "")) -> [messages_util:system_message(100129)];
		((Region == "") or (Version == "")) -> [messages_util:system_message(100129)];
		((Key == "") or (Secret == "")) -> [messages_util:system_message(100129)];
		(ActiveNew and (ActiveNew /= ActiveOld)) ->
			case (s3config_dao:already_has_active(Id)) of
				true ->
					[messages_util:system_message(100132)];
				_ -> 
					[]
			end;
	    true -> []
    end.

validate_to_delete(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

validate_to_restore(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

validate_to_trully_delete(Id) -> 
	if
        (Id < 1) -> [messages_util:system_message(412)];
	    true -> []
    end.

save(BodyPropList, OwnerId, Token) -> 
	BucketName = s3config_dataextractor:getbucket_name(BodyPropList, ""),
    BucketUrl = s3config_dataextractor:getbucket_url(BodyPropList, ""),
    Region = s3config_dataextractor:get_region(BodyPropList, ""),
    Version = s3config_dataextractor:get_version(BodyPropList, ""),
    Key = s3config_dataextractor:get_key(BodyPropList, ""),
    Secret = s3config_dataextractor:get_secret(BodyPropList, ""),
	Created = s3config_dao:create_object(BucketName, BucketUrl, Region, Version, Key, Secret, false, OwnerId),
	case lists:nth(1, Created) of 
		true ->
			Id = lists:nth(2, Created),
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_created(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(200)];
		_ ->
			[messages_util:system_message(100130)]
	end.

update(BodyPropList, Id, OwnerId, Token) -> 
	S3Config = lists:nth(1, edit(Id)),
	BucketName = s3config_dataextractor:getbucket_name(BodyPropList, 
													   proplists:get_value(<<"bucketName">>, S3Config, "")),
    BucketUrl = s3config_dataextractor:getbucket_url(BodyPropList, 
													 proplists:get_value(<<"bucketUrl">>, S3Config, "")),
    Region = s3config_dataextractor:get_region(BodyPropList,
											   proplists:get_value(<<"region">>, S3Config, "")),
    Version = s3config_dataextractor:get_version(BodyPropList, 
												 proplists:get_value(<<"version">>, S3Config, "")),
    Key = s3config_dataextractor:get_key(BodyPropList, 
										 proplists:get_value(<<"keyy">>, S3Config, "")),
    Secret = s3config_dataextractor:get_secret(BodyPropList, 
                                               proplists:get_value(<<"secret">>, S3Config, "")),
	Active = generic_dataextractor:get_bool(BodyPropList, <<"active">>, 
											proplists:get_value(<<"active">>, S3Config, false)),
	Updated = s3config_dao:update_object(Id, BucketName, BucketUrl, Region, Version, Key, Secret, Active),
	case Updated of 
		true ->
			ObjectNew = lists:nth(1, edit(Id)),
			applog_util:successfully_updated(OwnerId, S3Config, ObjectNew, object_class_name(), Token),
			[messages_util:system_message(201)];
		_ ->
			[messages_util:system_message(100131)]
	end.

edit(Id) -> 
	s3config_dao:load_object_by_id(Id).

load_all(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	s3config_dao:load_all_objects(Page, Rows, Conditions2, FlagAsDeleted).

load_all_for_public(BodyPropList, AdditionalConditions, Page, Rows) -> 
	ConditionsAndFlagAsDeleted = generic_dataextractor:get_conditions(BodyPropList),
	Conditions = lists:nth(1, ConditionsAndFlagAsDeleted),
	Conditions2 = lists:concat([Conditions, " ", AdditionalConditions]),
	FlagAsDeleted = lists:nth(2, ConditionsAndFlagAsDeleted),
	s3config_dao:load_all_objects_for_public(Page, Rows, Conditions2, FlagAsDeleted).

delete(Id, OwnerId, Token) -> 
	case s3config_dao:delete_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(204)];
		_ ->
			[messages_util:system_message(500)]
	end.

restore(Id, OwnerId, Token) -> 
	case s3config_dao:restore_object(Id) of 
		true ->
			Object = lists:nth(1, edit(Id)),
			applog_util:successfully_restored(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(207)];
		_ ->
			[messages_util:system_message(500)]
	end.

trully_delete(Id, OwnerId, Token) -> 
	Object = lists:nth(1, edit(Id)),
	case s3config_dao:trully_delete_object(Id) of 
		true ->
			applog_util:successfully_trully_deleted(OwnerId, Object, object_class_name(), Token),
			[messages_util:system_message(208)];
		_ ->
			[messages_util:system_message(500)]
	end.

authenticate(_BodyPropList, _Host, _Token) ->
	false.

logoff(_BodyPropList, _Token) ->
	false.

special_op1(_BodyPropList, _OwnerId, _Token) ->
	[messages_util:system_message(403)].

special_op2(_BodyPropList, _OwnerId, _Token) ->
	[messages_util:system_message(403)].
