-module(sntp_service).

-export([append_sntp/2]).

append_sntp(PropList, GetSntp) ->
	case GetSntp of 
		undefined ->
			PropList;
		_ ->
			UseSntp = lists:nth(1, GetSntp),
			case (UseSntp and (length(PropList) > 0)) of
				false ->
					PropList;
				_ ->
					MilliSeconds = get_right_timestamp(lists:nth(2, UseSntp)),
					[Header | Tail] = PropList,
					Header2 = [{"sntptimestamp", MilliSeconds} | Header],
					[Header2 | Tail]
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_right_timestamp(Language) ->
    LastDiff = sntp_dao:load_last_diff_by_language(Language),
	Now = erlang:system_time(millisecond),
	Undefined = datatransform_util:is_undefined(LastDiff),
	case Undefined of
		true ->
			Now;
		_ ->
			case number_util:is_nan(LastDiff) of
				true ->
					Now;
				_ -> 
					Now + LastDiff
			end
	end.
  















