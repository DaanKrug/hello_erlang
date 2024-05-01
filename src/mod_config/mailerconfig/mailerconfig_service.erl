-module(mailerconfig_service).

-export([load_configs_for_mailing/0, update_configs_after_mailed/1]).

load_configs_for_mailing() ->
    EmailConfigs = mailerconfig_dao:load_configs_for_mailing(),
    map_mailer_configurations(EmailConfigs, [], 1).

update_configs_after_mailed(EmailConfigurations) ->
	case (datatransform_util:is_emptylist(EmailConfigurations)) of
		true ->
			true;
		_ ->
			update_configs_by_run_usage(EmailConfigurations)
	end.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_configs_by_run_usage(EmailConfigurations) ->
	[EmailConfiguration | Tail] = EmailConfigurations,
    OwnerId = proplists:get_value(ownerId, EmailConfiguration, 0),
	Id = proplists:get_value(id, EmailConfiguration, 0),
	RunUsages = proplists:get_value(runUsages, EmailConfiguration, 0),
    update_configs_by_run_usage_array(OwnerId, Id, RunUsages),
	mailerconfig_dao:set_last_time_used(Id),
    update_configs_after_mailed(Tail).

update_configs_by_run_usage_array(OwnerId, Id, RunUsages) ->
	case (datatransform_util:is_emptylist(RunUsages)) of
		true ->
			true;
		_ ->
			[RunUsage | Tail] = RunUsages,
			Year = proplists:get_value(year, RunUsage, 0),
			Month = proplists:get_value(month, RunUsage, 0),
			Day = proplists:get_value(day, RunUsage, 0),
      	    Hour = proplists:get_value(hour, RunUsage, 0),
			Minute = proplists:get_value(minute, RunUsage, 0),
			Second = proplists:get_value(second, RunUsage, 0),
			Total = proplists:get_value(total, RunUsage, 0),
      		mailerconfigusedquota_dao:update_atual_usage(OwnerId, Id, Year, Month, Day, Hour, Minute, Second, Total),
      		update_configs_by_run_usage_array(OwnerId, Id, Tail)
	end.

map_mailer_configurations(EmailConfigs, EmailConfigurations, Position) ->
	case (Position > length(EmailConfigs)) of
		true ->
			lists:reverse(EmailConfigurations);
		_ ->
			EmailConfiguration = prepare_email_configuration(lists:nth(Position, EmailConfigs)),
			map_mailer_configurations(EmailConfigs, [EmailConfiguration | EmailConfigurations], Position + 1)
	end.
	
prepare_email_configuration(EmailConfig) ->
	DateListNow = date_util:date_time_now_proplist(0),
	Id = proplists:get_value(<<"id">>, EmailConfig, 0),
	OwnerId = proplists:get_value(<<"ownerId">>, EmailConfig, 0),
	EmailConfiguration = [
							{id, Id},
							{provider, proplists:get_value(<<"provider">>, EmailConfig, 0)},
							{senderName, proplists:get_value(<<"name">>, EmailConfig, 0)},
							{senderEmail, proplists:get_value(<<"username">>, EmailConfig, 0)},
							{senderPassword, proplists:get_value(<<"password">>, EmailConfig, 0)},
							{perMonth, proplists:get_value(<<"perMonth">>, EmailConfig, 0)},
							{perDay, proplists:get_value(<<"perDay">>, EmailConfig, 0)},
							{perHour, proplists:get_value(<<"perHour">>, EmailConfig, 0)},
							{perMinute, proplists:get_value(<<"perMinute">>, EmailConfig, 0)},
							{perSecond, proplists:get_value(<<"perSecond">>, EmailConfig, 0)},
							{lastTimeUsed, proplists:get_value(<<"lastTimeUsed">>, EmailConfig, 0)},
							{replayTo, proplists:get_value(<<"replayTo">>, EmailConfig, 0)},
							{userId, proplists:get_value(<<"userId">>, EmailConfig, 0)},
							{ownerId, OwnerId},
                            {isProvisioned, user_dao:is_admin_master(OwnerId) == false},
							{usedPerMonth, mailerconfigusedquota_dao:load_atual_usage_monthly(Id, DateListNow)},
							{usedPerDay, mailerconfigusedquota_dao:load_atual_usage_daily(Id, DateListNow)},
							{usedPerHour, mailerconfigusedquota_dao:load_atual_usage_hourly(Id, DateListNow)},
							{usedPerMinute, mailerconfigusedquota_dao:load_atual_usage_minutly(Id, DateListNow)},
							{usedPerSecond, mailerconfigusedquota_dao:load_atual_usage_secondly(Id, DateListNow)},
							{runUsages, []}
						 ],
	case (validate_initial_usage(EmailConfiguration)) of
		true ->
			EmailConfiguration;
		_ -> 
			[]
	end.

validate_initial_usage(EmailConfiguration) ->
	UsedPerMonth = proplists:get_value(usedPerMonth, EmailConfiguration, 0),
	PerMonth = proplists:get_value(perMonth, EmailConfiguration, 0),
    case (UsedPerMonth >= PerMonth) of
		true ->
			false;
		_ ->
			UsedPerDay = proplists:get_value(usedPerDay, EmailConfiguration, 0),
			PerDay = proplists:get_value(perDay, EmailConfiguration, 0),
			case (UsedPerDay >= PerDay) of
				true ->
					false;
				_ ->
					UsedPerHour = proplists:get_value(usedPerHour, EmailConfiguration, 0),
					PerHour = proplists:get_value(perHour, EmailConfiguration, 0),
					case (UsedPerHour >= PerHour) of
						true ->
							false;
						_ ->
							UsedPerMinute = proplists:get_value(usedPerMinute, EmailConfiguration, 0),
							PerMinute = proplists:get_value(perMinute, EmailConfiguration, 0),
							case (UsedPerMinute >= PerMinute) of
								true ->
									false;
								_ ->
									true
							end
					end
			end
	end.