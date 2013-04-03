-module(skyraid_webmachine_test).

-include_lib("eunit/include/eunit.hrl").

-define(T(TestName), {atom_to_list(TestName), fun TestName/0}).

skyraid_webmachine_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			?T(get_all_users)
		] 
	}.

setup() ->
	ok =skyraid:start(),
	ok = skyraid_webmachine:start().

teardown(_Any) ->
	ok = skyraid:stop(),
	ok = skyraid_webmachine:stop().

get_all_users() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, body}} = httpc:request("http://localhost:8000/api/user").
