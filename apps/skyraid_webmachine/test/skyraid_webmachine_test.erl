-module(skyraid_webmachine_test).

-include_lib("skyraid/include/skyraid.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(T(TestName), {atom_to_list(TestName), fun TestName/0}).

skyraid_webmachine_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			?T(login_normal),
			?T(login_no_accounts),
			?T(login_invalid_password),
			?T(login_invalid_username_password),

			?T(create_user_normal),
			?T(create_user_existing_username),
			?T(get_all_users),

			?T(get_accounts_normal),
			?T(get_account_normal),
			?T(get_all_accounts)
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
	{200,[	{<<"status">>,<<"ok">>},
          	{<<"sessionId">>,<<"<0.204.0>">>},
			{<<"user">>,{struct,[{<<"displayName">>,<<"AdamDisplay">>},{<<"email">>,<<"adam@gmail.com">>}]}},
            {<<"accounts">>,{struct,[{<<"id">>,<<"0.0">>},{<<"name">>,<<"AdamAccount1">>},{<<"provider">>,<<"ftp">>}]}}]} = rest_req(post, "http://localhost:8000/api/login", Login).

login_no_accounts() ->
	Login = "{\"username\":\"Eva\", \"password\": \"test\"}",
	{200,[{<<"status">>,<<"ok">>},
		  {<<"sessionId">>,_},
          {<<"user">>,{struct,[{<<"displayName">>,<<"EvaDisplay">>},{<<"email">>,<<"adam@gmail.com">>}]}},
          {<<"accounts">>,[]}]} = rest_req(post, "http://localhost:8000/api/login", Login).

login_invalid_password() ->
	Login = "{\"username\":\"Adam\", \"password\": \"WrongPassword\"}",
	{500, [{<<"status">>,<<"error">>},{<<"error">>,<<"invalid_password">>}]} = rest_req(post, "http://localhost:8000/api/login", Login).

login_invalid_username_password() ->
	Login = "{\"username\":\"NoUser\", \"password\": \"WrongPassword\"}",
	{500, [{<<"status">>,<<"error">>},{<<"error">>,<<"invalid_username_password">>}]} = rest_req(post, "http://localhost:8000/api/login", Login).

create_user_normal() ->
	User = "{\"username\":\"NewUser\", \"password\": \"test\", \"email\": \"my.mail@gmail.com\"}",
	{200, [{<<"status">>,<<"ok">>}, {<<"userId">>, _UserId }]} = rest_req(put, "http://localhost:8000/api/user", User).

create_user_existing_username() ->
	User = "{\"username\":\"Adam\", \"password\": \"test\", \"email\": \"my.mail@gmail.com\" }",
	{500, [{<<"status">>,<<"error">>}, {<<"error">>, <<"username_exist">> }]} = rest_req(put, "http://localhost:8000/api/user", User).

get_all_users() ->
	{200, _User} = rest_req("http://localhost:8000/api/user").

get_all_accounts() ->
	{200, [{<<"status">>,<<"ok">>}, {<<"accounts">>, _Accounts }]} = rest_req("http://localhost:8000/api/account").

get_accounts_normal() ->
	{200, [{<<"status">>,<<"ok">>}, {<<"accounts">>, _Accounts }]} = rest_req("http://localhost:8000/api/account/0").

get_account_normal() ->
	{200, [{<<"status">>,<<"ok">>}, {<<"accounts">>, _Accounts }]} = rest_req("http://localhost:8000/api/account/0/0").

rest_req(URL) ->
	{ok, {{_V, ReturnCode, _R}, _H, ResponseBody}} = httpc:request(URL),
	Response = case mochijson2:decode(ResponseBody) of
					{struct, ResponseTerm} -> ResponseTerm;
					ResponseTerm -> ResponseTerm
				end,
	{ReturnCode, Response}.

rest_req(Method, URL, Body) ->
	Header = [],
	Type = "application/json",
	HTTPOptions = [],
	Options = [],
	{ok, {{_V, ReturnCode, _R}, _H, ResponseBody}} = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
	{struct, ResponseTerm} = mochijson2:decode(ResponseBody),
	{ReturnCode, ResponseTerm}.
