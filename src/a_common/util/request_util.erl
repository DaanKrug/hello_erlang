-module(request_util).

-export([enable_rest_and_cors/1, get_host/1, get_body/1]).

enable_rest_and_cors(Req) ->
	Header1 = <<"access-control-allow-methods">>,
	HeaderValues1 = <<"GET, OPTIONS, HEAD, POST, PUT, PATCH, DELETE">>,
	Header2 = <<"access-control-allow-origin">>,
	HeaderValues2 = <<"*">>,
	Req1 = cowboy_req:set_resp_header(Header1, HeaderValues1, Req),
    cowboy_req:set_resp_header(Header2, HeaderValues2, Req1).

get_host(Req) ->
	ForwardedHost = cowboy_req:header("x-forwarded-for", Req),
	case ForwardedHost of
		undefined ->
			cowboy_req:host(Req);
		_ -> 
			ForwardedHost
	end.

get_body(Req) ->
	{ok, Body, _Req1} = cowboy_req:read_body(Req),
	Res = json_util:decode(Body),
	lists:nth(1, tuple_to_list(Res)).