-module(math_util).

-export([
		 pow/2,
		 get_precision/1,
		 remainder/2
		 ]
	   ).

%%==============================================================================
%% @doc Get precision of float
%% @end
-spec get_precision(float()) -> integer().
get_precision(Float) when is_float(Float) ->
    case Float < 0 of
       false -> get_precision(Float, Float, left_floor(Float), 1);
       true  -> get_precision(-Float, -Float, left_floor(-Float), 1)
    end.
	
%%==============================================================================
%% @doc Normalized math:rem operation
%% @end
-spec remainder(integer(),integer()) -> integer().
remainder(Number,Divisor) ->
	if
      (Divisor == 0) -> 0;
	  (Number == 0) -> 0;
	  (Divisor < 0) -> remainder(Number,Divisor * -1);
	  (Number < 0) -> remainder(Number * -1,Divisor);
      true -> (Number rem Divisor)
    end.
	
%%==============================================================================
%% @doc optimized math:pow operation
%% @end
-spec pow(integer(),integer()) -> integer().
pow(Base,Expoent) ->
	if
      (Expoent == 0) -> 1;
      ((Expoent rem 2) == 1) -> Base * pow(Base,Expoent - 1);
      true -> pow_even(Base,Expoent)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    private functions       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_precision(OriginalFloat, Float, Integer, Precision) when is_float(Float) ->
	IntegerTruncatedBy10 = trunc(Integer/10)/(pow(10,Precision - 2)),
    if 
		(Float == Integer) -> Precision - 1;  
        (IntegerTruncatedBy10 == OriginalFloat) -> Precision - 2;
        true -> get_precision(OriginalFloat, Float * 10, left_floor(Float * 10), Precision + 1)
    end.

pow_even(Base,Expoent) ->
    Result = pow(Base,(Expoent div 2)),
    Result * Result.

left_floor(Float) when is_float(Float) ->
    Truncated = trunc(Float),
    case (Float - Truncated) of
        Negative when Negative < 0 -> Truncated;
        Positive when Positive > 0 -> Truncated + 1;
        _ -> Truncated
    end.
