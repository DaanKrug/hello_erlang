-module(mailerconfig_util).

-export(
   		[
		 mark_email_configuration_as_used/2, 
		 email_configuration_can_be_used/3,
		 get_year_of_run_usage/1,
		 get_month_of_run_usage/1,
		 get_day_of_run_usage/1,
		 get_hour_of_run_usage/1,
		 get_minute_of_run_usage/1,
		 get_second_of_run_usage/1,
		 calculate_montly_run_usage/3,
		 calculate_daily_run_usage/3,
		 calculate_hourly_run_usage/3,
		 calculate_minutly_run_usage/3,
		 calculate_secondly_run_usage/3,
		 adjust_run_usage_config/4,
		 add_run_usage_config/4,
		 new_run_usage/0
		]
	   ).

mark_email_configuration_as_used(EmailConfigurations, UsedPosition) ->
	case (UsedPosition > 0) of
		true -> 
			adjust_run_usage_config(EmailConfigurations, UsedPosition, [], 1);
		_ ->
			EmailConfigurations
	end.

email_configuration_can_be_used(EmailConfiguration, OwnerId, UseProvisioned) ->
	UserId = number_util:to_integer(proplists:get_value(userId, EmailConfiguration, 0)),
	Provisioned = proplists:get_value(provisioned, EmailConfiguration, 0),
	case validate_user_ownership(OwnerId, UserId, Provisioned, UseProvisioned) of
		true ->
			LastTimeUsed = number_util:to_integer(proplists:get_value(lastTimeUsed, EmailConfiguration, 0)),
			DateListNow = date_util:date_time_now_proplist(0),
			DateListUsed = date_util:now_millis_to_date_time_now_proplist(LastTimeUsed),
			RunUsages = proplists:get_value(runUsages, EmailConfiguration, []),
			UsedPerMonth = get_used_per_month(EmailConfiguration, DateListUsed, DateListNow),
			UsedPerMonth2 = UsedPerMonth + calculate_montly_run_usage(RunUsages, DateListNow, 0) + 1,
			PerMonth = number_util:to_integer(proplists:get_value(perMonth, EmailConfiguration, 0)),
			case (UsedPerMonth2 > PerMonth) of 
				true ->
					false;
				_ ->
					UsedPerDay = get_used_per_day(EmailConfiguration, DateListUsed, DateListNow),
					UsedPerDay2 = UsedPerDay + calculate_daily_run_usage(RunUsages, DateListNow, 0) + 1,
					PerDay = number_util:to_integer(proplists:get_value(perDay, EmailConfiguration, 0)),
					case (UsedPerDay2 > PerDay) of 
						true ->
							false;
						_ ->
							UsedPerHour = get_used_per_hour(EmailConfiguration, DateListUsed, DateListNow),
							UsedPerHour2 = UsedPerHour  + calculate_hourly_run_usage(RunUsages, DateListNow, 0) + 1,
							PerHour = number_util:to_integer(proplists:get_value(perHour, EmailConfiguration, 0)),
							case ((PerHour > 0) and (UsedPerHour2 > PerHour)) of
								true ->
									false;
								_ ->
									UsedPerMinute = get_used_per_minute(EmailConfiguration, DateListUsed, DateListNow),
									UsedPerMinute2 = UsedPerMinute + calculate_minutly_run_usage(RunUsages, DateListNow, 0) + 1,
									PerMinute = number_util:to_integer(proplists:get_value(perMinute, EmailConfiguration, 0)),
									case ((PerMinute > 0) and (UsedPerMinute2 > PerMinute)) of
										true ->
											false;
										_ ->
											UsedPerSecond = get_used_per_second(EmailConfiguration, DateListUsed, DateListNow),
											UsedPerSecond2 = UsedPerSecond + calculate_secondly_run_usage(RunUsages, DateListNow, 0) + 1,
											PerSecond = number_util:to_integer(proplists:get_value(perSecond, EmailConfiguration, 0)),
											(PerSecond == 0) or (PerSecond > UsedPerSecond2)
									end
							end
					end
			end;
		_ ->
			false
	end.

get_year_of_run_usage(RunUsage) ->
    number_util:to_integer(proplists:get_value(year, RunUsage, 0)).

get_month_of_run_usage(RunUsage) ->
    number_util:to_integer(proplists:get_value(month, RunUsage, 0)).
  
get_day_of_run_usage(RunUsage) ->
    number_util:to_integer(proplists:get_value(day, RunUsage, 0)).
  
get_hour_of_run_usage(RunUsage) ->
    number_util:to_integer(proplists:get_value(hour, RunUsage, 0)).
  
get_minute_of_run_usage(RunUsage) ->
    number_util:to_integer(proplists:get_value(minute, RunUsage, 0)).
  
get_second_of_run_usage(RunUsage) ->
    number_util:to_integer(proplists:get_value(second, RunUsage, 0)).
  
get_total_of_run_usage(RunUsage) ->
    number_util:to_integer(proplists:get_value(total, RunUsage, 0)).

calculate_secondly_run_usage(RunUsages, DateList, Total) ->
	case (datatransform_util:is_emptylist(RunUsages)) of
		true ->
			Total;
		_ ->
			[RunUsage | Tail] = RunUsages,
			case (equals_second_of_run_usage(RunUsage, DateList)) of
				true ->
					Total2 = Total + get_total_of_run_usage(RunUsage),
					calculate_secondly_run_usage(Tail, DateList, Total2);
				_ ->
					calculate_secondly_run_usage(Tail, DateList, Total)
			end
	end.

calculate_minutly_run_usage(RunUsages, DateList, Total) ->
	case (datatransform_util:is_emptylist(RunUsages)) of
		true ->
			Total;
		_ ->
			[RunUsage | Tail] = RunUsages,
			case (equals_minute_of_run_usage(RunUsage, DateList)) of
				true ->
					Total2 = Total + get_total_of_run_usage(RunUsage),
					calculate_minutly_run_usage(Tail, DateList, Total2);
				_ ->
					calculate_minutly_run_usage(Tail, DateList, Total)
			end
	end.

calculate_hourly_run_usage(RunUsages, DateList, Total) ->
	case (datatransform_util:is_emptylist(RunUsages)) of
		true ->
			Total;
		_ ->
			[RunUsage | Tail] = RunUsages,
			case (equals_hour_of_run_usage(RunUsage, DateList)) of
				true ->
					Total2 = Total + get_total_of_run_usage(RunUsage),
					calculate_hourly_run_usage(Tail, DateList, Total2);
				_ ->
					calculate_hourly_run_usage(Tail, DateList, Total)
			end
	end.

calculate_daily_run_usage(RunUsages, DateList, Total) ->
	case (datatransform_util:is_emptylist(RunUsages)) of
		true ->
			Total;
		_ ->
			[RunUsage | Tail] = RunUsages,
			case (equals_day_of_run_usage(RunUsage, DateList)) of
				true ->
					Total2 = Total + get_total_of_run_usage(RunUsage),
					calculate_daily_run_usage(Tail, DateList, Total2);
				_ ->
					calculate_daily_run_usage(Tail, DateList, Total)
			end
	end.

calculate_montly_run_usage(RunUsages, DateList, Total) ->
	case (datatransform_util:is_emptylist(RunUsages)) of
		true ->
			Total;
		_ ->
			[RunUsage | Tail] = RunUsages,
			case (equals_month_of_run_usage(RunUsage, DateList)) of
				true ->
					Total2 = Total + get_total_of_run_usage(RunUsage),
					calculate_montly_run_usage(Tail, DateList, Total2);
				_ ->
					calculate_montly_run_usage(Tail, DateList, Total)
			end
	end.

adjust_run_usage_config(EmailConfigurations, UsedPosition, EmailConfigurationsNew, Position) ->
	case (Position > length(EmailConfigurations)) of
		true ->
			EmailConfigurationsNew;
		_ ->
			EmailConfigurationsNew2 = add_run_usage_config(EmailConfigurations, EmailConfigurationsNew, 
														   Position, (UsedPosition == Position)),
			adjust_run_usage_config(EmailConfigurations, UsedPosition, EmailConfigurationsNew2, Position + 1)
	end.

add_run_usage_config(EmailConfigurations, EmailConfigurationsNew, Position, Used) ->
	case Used of
		true ->
		    EmailConfiguration = lists:nth(Position, EmailConfigurations),
			RunUsages = proplists:get_value(runUsages, EmailConfiguration, []),
			EmailConfiguration2 = proplists:delete(runUsages, EmailConfiguration),
			RunUsages2 = [new_run_usage() | lists:reverse(RunUsages)],
		    EmailConfiguration3 = [{runUsages, lists:reverse(RunUsages2)} | EmailConfiguration2],
			lists:reverse([EmailConfiguration3 | EmailConfigurationsNew]);
		_ ->
			lists:reverse([lists:nth(Position, EmailConfigurations) | EmailConfigurationsNew])
	end.

new_run_usage() ->
	DateListNow = date_util:date_time_now_proplist(0),
	[
		{year, proplists:get_value(year, DateListNow, 0)},
		{month, proplists:get_value(month, DateListNow, 0)},
		{day, proplists:get_value(day, DateListNow, 0)},
		{hour, proplists:get_value(hour, DateListNow, 0)},
		{minute, proplists:get_value(minute, DateListNow, 0)},
		{second, proplists:get_value(second, DateListNow, 0)},
		{total, 1}
	].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_used_per_month(EmailConfiguration, DateList1, DateList2) ->
	case (equals_month_of_run_usage(DateList1, DateList2)) of
		true ->
			number_util:to_integer(proplists:get_value(usedPerMonth, EmailConfiguration, 0));
		_ ->
			0
	end.

get_used_per_day(EmailConfiguration, DateList1, DateList2) ->
	case (equals_day_of_run_usage(DateList1, DateList2)) of
		true ->
			number_util:to_integer(proplists:get_value(usedPerDay, EmailConfiguration, 0));
		_ ->
			0
	end.

get_used_per_hour(EmailConfiguration, DateList1, DateList2) ->
	case (equals_hour_of_run_usage(DateList1, DateList2)) of
		true ->
			number_util:to_integer(proplists:get_value(usedPerHour, EmailConfiguration, 0));
		_ ->
			0
	end.

get_used_per_minute(EmailConfiguration, DateList1, DateList2) ->
	case (equals_minute_of_run_usage(DateList1, DateList2)) of
		true ->
			number_util:to_integer(proplists:get_value(usedPerMinute, EmailConfiguration, 0));
		_ ->
			0
	end.

get_used_per_second(EmailConfiguration, DateList1, DateList2) ->
	case (equals_second_of_run_usage(DateList1, DateList2)) of
		true ->
			number_util:to_integer(proplists:get_value(usedPerSecond, EmailConfiguration, 0));
		_ ->
			0
	end.

validate_user_ownership(OwnerId, UserId, Provisioned, UseProvisioned) ->
    Sender = user_dao:load_object_by_id(OwnerId),
    SenderCategory = proplists:get_value(<<"category">>, Sender, ""),
    SenderOwnerId = number_util:to_integer(proplists:get_value(<<"ownerId">>, Sender, 0)),
	case (Provisioned /= UseProvisioned) of
		true ->
			false;
		_ ->
			AdminMaster = (UserId == OwnerId) and string_util:equals(SenderCategory, <<"admin_master">>),
			CanUseNotProvisioned = ((OwnerId == 0) or AdminMaster),
			Forbidden1 = (Provisioned == false) and (CanUseNotProvisioned == false),
			Forbidden2 = string_util:equals(SenderCategory, <<"external">>) and (UserId /= SenderOwnerId),
			Forbidden3 = UseProvisioned and (UserId /= OwnerId) and (UserId /= SenderOwnerId),
			(Forbidden1 or Forbidden2 or Forbidden3) == false
	end.

equals_second_of_run_usage(RunUsage, DateList) ->
	Second = proplists:get_value(second, DateList, 0),
	case (get_second_of_run_usage(RunUsage) == Second) of
		true ->
			equals_minute_of_run_usage(RunUsage, DateList);
		_ ->
			false
	end.

equals_minute_of_run_usage(RunUsage, DateList) ->
	Minute = proplists:get_value(minute, DateList, 0),
	case (get_minute_of_run_usage(RunUsage) == Minute) of
		true ->
			equals_hour_of_run_usage(RunUsage, DateList);
		_ ->
			false
	end.

equals_hour_of_run_usage(RunUsage, DateList) ->
	Hour = proplists:get_value(hour, DateList, 0),
	case (get_hour_of_run_usage(RunUsage) == Hour) of
		true ->
			equals_day_of_run_usage(RunUsage, DateList);
		_ ->
			false
	end.

equals_day_of_run_usage(RunUsage, DateList) ->
	Day = proplists:get_value(day, DateList, 0),
	case (get_day_of_run_usage(RunUsage) == Day) of
		true ->
			equals_month_of_run_usage(RunUsage, DateList);
		_ ->
			false
	end.

equals_month_of_run_usage(RunUsage, DateList) ->
	SameYear = get_year_of_run_usage(RunUsage) == proplists:get_value(year, DateList, 0),
	SameMonth = get_year_of_run_usage(RunUsage) == proplists:get_value(month, DateList, 0),
	(SameMonth and SameYear).













