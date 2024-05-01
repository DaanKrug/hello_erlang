-module(return_util).

-export([
		 get_operation_error/1, 
		 get_operation_success/2, 
		 get_validation_result/2, 
		 get_report/1
		]).

get_operation_error(Message) -> 
	Message2 = string_util:to_string(["", Message]),
	[
	 {<<"objectClass">>, <<"OperationError">>}, 
     {<<"code">>, 500}, 
     {<<"msg">>, Message2} 
	].

get_operation_success(Code, Message) ->
	Message2 = string_util:to_string(["", Message]),
	[
	 {<<"objectClass">>, <<"OperationSuccess">>}, 
     {<<"code">>, Code}, 
     {<<"msg">>, Message2} 
	].
  
get_validation_result(Code, Message) ->
	Message2 = string_util:to_string(["", Message]),
    [
	 {<<"objectClass">>, <<"ValidationResult">>}, 
     {<<"code">>, Code}, 
     {<<"msg">>, Message2} 
	].
  
get_report(Html) ->
	Html2 = string_util:to_string(["", Html]),
    [
	 {<<"objectClass">>, <<"Report">>}, 
     {<<"code">>, 205}, 
     {<<"msg">>, Html2} 
	].