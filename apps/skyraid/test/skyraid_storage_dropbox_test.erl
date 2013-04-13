-module(skyraid_storage_dropbox_test).

-include("skyraid.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(T(TestName), {atom_to_list(TestName), fun TestName/1}).

skyraid_storage_dropbox_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			?T(account_info_normal)
		]
	}.

setup() ->
	ok = skyraid:start(),
	{ok, #skr_auth_reqtoken{url=URL}=RT} = skyraid_storage_dropbox:authorize_url(),
	launch_user_authentication(URL),
	{ok, AT} = skyraid_storage_dropbox:access_token(RT),
	AT.

teardown(_) ->
	skyraid:stop().

account_info_normal(AT) ->
	skyraid_storage_dropbox:account_info(AT).

launch_user_authentication(URL) ->
	process_flag(trap_exit, true),
	Cmd = case os:type() of 
			{win, _Osname} -> "start ";
			{unix, _Osname} -> "epiphany "
		end,

	try open_port({spawn, Cmd ++ URL},[binary,{line, 255}]) of
		Port -> await_user(Port)
	catch
		_ -> erlang:display("Exception catched")
	end.

await_user(Port) ->
	receive 
		{'EXIT', Port, _} -> ok;
		_ -> await_user(Port)
	end. 