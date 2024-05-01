-module(mailerconfigusedquota_dao).

-export(
        [
		 get_database_name/0, 
		 update_atual_usage/9,
		 load_atual_usage_monthly/2,
		 load_atual_usage_daily/2,
		 load_atual_usage_hourly/2,
		 load_atual_usage_minutly/2,
		 load_atual_usage_secondly/2
		]
	   ).

get_database_name() ->
	lists:concat([base_dao:get_database_prefix_name(), "_billingcontrol"]).

update_atual_usage(OwnerId, MailerConfigId, Year, Month, Day, Hour, Minute, Second, Total) ->
    Sql = lists:concat([
						"select id from mailerconfigusedquota where ownerId = ? and mailerConfigId = ? ", 
						"and year = ? and month = ? and day = ? and hour = ? ",
						"and minute = ? and second = ? limit 1"
						]),
    ParamValues = [OwnerId, MailerConfigId, Year, Month, Day, Hour, Minute, Second],
	Usages = base_dao:load_by_params(Sql, ParamValues, ?MODULE),
	case (length(Usages) > 0) of
		true ->
			Id = proplists:get_value(<<"id">>, lists:nth(1, Usages), 0),
			update(Id, Total);
		_ ->
			create(MailerConfigId, Year, Month, Day, Hour, Minute, Second, Total, OwnerId)
	end.

load_atual_usage_monthly(MailerConfigId, DateList) ->
    Sql = lists:concat([
						"select sum(amount) as amount from mailerconfigusedquota where mailerConfigId = ? ",
						"and year = ? and month = ?"
						]),
	ParamValues = [
				   MailerConfigId, 
				   proplists:get_value(year, DateList, 0), 
				   proplists:get_value(month, DateList, 0)
				  ],
    Result = base_dao:load_by_params(Sql, ParamValues, ?MODULE),
	number_util:to_integer(lists:nth(1, Result)).
  
load_atual_usage_daily(MailerConfigId, DateList) ->
	Sql = lists:concat([
						"select sum(amount) as amount from mailerconfigusedquota where mailerConfigId = ? ",
						"and year = ? and month = ? and day = ?"
						]),
	ParamValues = [
				   MailerConfigId, 
				   proplists:get_value(year, DateList, 0), 
				   proplists:get_value(month, DateList, 0),
				   proplists:get_value(day, DateList, 0)
				  ],
    Result = base_dao:load_by_params(Sql, ParamValues, ?MODULE),
	number_util:to_integer(lists:nth(1, Result)).

load_atual_usage_hourly(MailerConfigId, DateList) ->
    Sql = lists:concat([
						"select sum(amount) as amount from mailerconfigusedquota where mailerConfigId = ? ",
						"and year = ? and month = ? and day = ? and hour = ?"
						]),
	ParamValues = [
				   MailerConfigId, 
				   proplists:get_value(year, DateList, 0), 
				   proplists:get_value(month, DateList, 0),
				   proplists:get_value(day, DateList, 0),
				   proplists:get_value(hour, DateList, 0)
				  ],
    Result = base_dao:load_by_params(Sql, ParamValues, ?MODULE),
	number_util:to_integer(lists:nth(1, Result)).
  
load_atual_usage_minutly(MailerConfigId, DateList) ->
    Sql = lists:concat([
						"select sum(amount) as amount from mailerconfigusedquota where mailerConfigId = ? ",
						"and year = ? and month = ? and day = ? and hour = ? and minute = ?"
						]),
	ParamValues = [
				   MailerConfigId, 
				   proplists:get_value(year, DateList, 0), 
				   proplists:get_value(month, DateList, 0),
				   proplists:get_value(day, DateList, 0),
				   proplists:get_value(hour, DateList, 0),
				   proplists:get_value(minute, DateList, 0)
				  ],
    Result = base_dao:load_by_params(Sql, ParamValues, ?MODULE),
	number_util:to_integer(lists:nth(1, Result)).
  
load_atual_usage_secondly(MailerConfigId, DateList) ->
    Sql = lists:concat([
						"select sum(amount) as amount from mailerconfigusedquota where mailerConfigId = ? ",
						"and year = ? and month = ? and day = ? and hour = ? and minute = ?  and second = ?"
						]),
	ParamValues = [
				   MailerConfigId, 
				   proplists:get_value(year, DateList, 0), 
				   proplists:get_value(month, DateList, 0),
				   proplists:get_value(day, DateList, 0),
				   proplists:get_value(hour, DateList, 0),
				   proplists:get_value(minute, DateList, 0),
				   proplists:get_value(second, DateList, 0)
				  ],
    Result = base_dao:load_by_params(Sql, ParamValues, ?MODULE),
	number_util:to_integer(lists:nth(1, Result)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update(Id, Total) ->
    Sql = "update mailerconfigusedquota set amount = (amount + ?) where id = ?",
    base_dao:update(Sql, [Total, Id], ?MODULE).

create(MailerConfigId, Year, Month, Day, Hour, Minute, Second, Total, OwnerId) ->
    Sql = lists:concat([
						"insert into mailerconfigusedquota(mailerConfigId, year, month, ", 
						"day, hour, minute, second, amount, ownerId) values(?,?,?,?,?,?,?,?,?)"
						]),
	ParamValues = [MailerConfigId, Year, Month, Day, Hour, Minute, Second, Total, OwnerId],
    base_dao:create(Sql, ParamValues, ?MODULE).
  