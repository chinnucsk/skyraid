-module(skyraid_webmachine_test).

-include_lib("eunit/include/eunit.hrl").

-define(T(TestName), {atom_to_list(TestName), fun TestName/0}).

skyraid_webmachine_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			?T(login_normal),
			?T(login_invalid_password),
			?T(login_invalid_username_password),
			?T(get_all_users)
		] 
	}.

setup() ->
	application:set_env(skyraid_webmachine, ip, "127.0.0.1"),
	application:set_env(skyraid_webmachine, port, 8000),
	ok = skyraid_webmachine:start().

teardown(_Any) ->
	ok = skyraid_webmachine:stop().

login_normal() ->
	Login = "{\"username\":\"Adam\", \"password\": \"test\"}",
	[{<<"status">>,<<"ok">>},{<<"session">>, _Session},{<<"user">>,"Adam"}] = rest_req(post, "http://localhost:8000/api/login", Login).

login_invalid_password() ->
	Login = "{\"username\":\"Adam\", \"password\": \"WrongPassword\"}",
	[{<<"status">>,<<"error">>},{<<"error">>,<<"invalid_password">>}] = rest_req(post, "http://localhost:8000/api/login", Login).

login_invalid_username_password() ->
	Login = "{\"username\":\"NoUser\", \"password\": \"WrongPassword\"}",
	[{<<"status">>,<<"error">>},{<<"error">>,<<"invalid_username_password">>}] = rest_req(post, "http://localhost:8000/api/login", Login).

get_all_users() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} = httpc:request("http://localhost:8000/api/user").

rest_req(Method, URL, Body) ->
	Header = [],
	Type = "application/json",
	HTTPOptions = [],
	Options = [],
	{ok, {{_V, 200, _R}, _H, ResponseBody}} = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
	{struct, ResponseTerm} = mochijson2:decode(ResponseBody),
	ResponseTerm.
