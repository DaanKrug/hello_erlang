-module(date_util).

-export(
   		[
		 now_millis_to_date_time_now_proplist/1, 
		 date_time_now_proplist/1,
		 time_to_sql_datetime/2,
		 now/1
		]
	   ).

now_millis_to_date_time_now_proplist(Milliseconds) ->
	Now = calendar:now_to_datetime(now_now_from_milliseconds(Milliseconds)),
	[Header | Tail] = tuple_to_list(Now),
	Date = tuple_to_list(Header),
	Time = lists:nth(1, Tail),
	[
	 {year, number_util:to_integer(lists:nth(1, Date))},
	 {month, number_util:to_integer(lists:nth(2, Date))},
	 {day, number_util:to_integer(lists:nth(3, Date))},
	 {hour, number_util:to_integer(lists:nth(1, Time))},
	 {minute, number_util:to_integer(lists:nth(2, Time))},
	 {second, number_util:to_integer(lists:nth(3, Time))}
    ].

date_time_now_proplist(IncrementDecrementMillis) ->
	Now = calendar:now_to_datetime(now_now(IncrementDecrementMillis)),
	[Header | Tail] = tuple_to_list(Now),
	Date = tuple_to_list(Header),
	Time = lists:nth(1, Tail),
	[
	 {year, number_util:to_integer(lists:nth(1, Date))},
	 {month, number_util:to_integer(lists:nth(2, Date))},
	 {day, number_util:to_integer(lists:nth(3, Date))},
	 {hour, number_util:to_integer(lists:nth(1, Time))},
	 {minute, number_util:to_integer(lists:nth(2, Time))},
	 {second, number_util:to_integer(lists:nth(3, Time))}
    ].

time_to_sql_datetime(IncrementDecrementMillis, AsString) ->
	Now = calendar:now_to_datetime(now_now(IncrementDecrementMillis)),
	[Header | Tail] = tuple_to_list(Now),
	Date = tuple_to_list(Header),
	Time = tuple_to_list(lists:nth(1, Tail)),
	Year = lists:nth(1, Date),
	Month = lists:nth(2, Date),
	Day = lists:nth(3, Date),
	Hour = lists:nth(1, Time),
	Minute = lists:nth(2, Time),
	Second = lists:nth(3, Time),
	case (AsString /= true) of
		true ->
			{{Year, Month, Day}, {Hour, Minute, Second}};
		_ ->
			Month2 = string_util:to_list(string_util:left_zeros(Month, 2)),
			Day2 = string_util:to_list(string_util:left_zeros(Day, 2)),
			Hour2 = string_util:to_list(string_util:left_zeros(Hour, 2)),
			Minute2 = string_util:to_list(string_util:left_zeros(Minute, 2)),
			Second2 = string_util:to_list(string_util:left_zeros(Second, 2)),
			lists:concat([Year, "-", Month2, "-", Day2, " ", Hour2, ":", Minute2, ":", Second2])
	end.
	
now(IncrementDecrementMillis) ->
	Summer = IncrementDecrementMillis * 1000,
    erlang:system_time(millisecond) - Summer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
now_now(IncrementDecrementMillis) ->
	Summer = IncrementDecrementMillis * 1000,
    ErlangSystemTime = erlang:system_time(microsecond) - Summer,
    MegaSecs = (ErlangSystemTime div 1000_000_000_000),
    Secs = (ErlangSystemTime div 1000_000) - (MegaSecs * 1000_000),
    MicroSecs = (ErlangSystemTime rem 1000_000),
    {MegaSecs, Secs, MicroSecs}.

now_now_from_milliseconds(Milliseconds) ->
    MegaSecs = (Milliseconds div 1000_000_000),
    Secs = (Milliseconds div 1000_000) - (MegaSecs * 1000_000),
    MicroSecs = (Milliseconds rem 1000_000),
    {MegaSecs, Secs, MicroSecs}.

