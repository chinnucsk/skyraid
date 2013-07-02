-module(skyraid_webmachine_rest).

-export([get/1, get/2, rest_req/1, rest_req/2, rest_req/3, rest_req/4, rest_req/5]).

get(URL) ->
	rest_req(json, URL). 

get(URL, Header) ->
	rest_req(get, URL, Header, "application/json", "apa").

rest_req(URL) ->
	rest_req(json, URL).

rest_req(text, URL) ->
	{ok, {{_V, ReturnCode, _R}, _H, ResponseBody}} = httpc:request(URL),
	{ReturnCode, ResponseBody};

rest_req(json, URL) ->
	{ok, {{_V, ReturnCode, _R}, _H, ResponseBody}} = httpc:request(URL),
	Response = case mochijson2:decode(ResponseBody) of
					{struct, ResponseTerm} -> ResponseTerm;
					ResponseTerm -> ResponseTerm
				end,
	{ReturnCode, Response}.

rest_req(Method, URL, Body) ->
	rest_req(Method, URL, "application/json", Body).

rest_req(Method, URL, "application/json" = Type, Body) ->
	Header = [],
	HTTPOptions = [],
	Options = [],
	{ok, {{_V, ReturnCode, _R}, _H, ResponseBody}} = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
	{struct, ResponseTerm} = mochijson2:decode(ResponseBody),
	{ReturnCode, ResponseTerm};

rest_req(Method, URL, Type, Body) ->
	rest_req(Method, URL, [], Type, Body).

rest_req(Method, URL, Header, Type, Body) ->
	HTTPOptions = [],
	Options = [],
	{ok, {{_V, ReturnCode, _R}, _H, ResponseBody}} = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
	{ReturnCode, ResponseBody}.