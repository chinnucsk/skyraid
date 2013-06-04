-module(skyraid_demo).
-include("skyraid.hrl").

-export([run/0, run/2]).

run() ->
	skyraid:register(#skr_user{username = <<"Test">>, password = <<"test">>}),
	{ok, SessionRef} = skyraid:login(<<"Test">>, <<"test">>),
	{ok, #skr_auth_reqtoken{url=URL}=RT} = skyraid:authenticate(dropbox),
	launch_user_authentication(URL),
	{ok, _} = skyraid:add_account(SessionRef, RT),
	{ok, _Files} = skyraid:file_list(SessionRef, dropbox),
	{ok, _File} = skyraid:file_read(SessionRef, dropbox, "master_slave.erl", []),
	{ok, _MetaData} = skyraid:file_write(SessionRef, dropbox, "howdy.txt", <<"Writing to dropbox">>, []),
	{ok, _} = skyraid:add_account(SessionRef, #skr_account{
						     storage_id=ftp,
						     authentication=#skr_auth_basic{url="ftp://myftp:8080", provider=ftp, username= <<"Testing">>, password= <<"Test">>}}),
    {ok, _} = skyraid:file_list(SessionRef, ftp).

run(Provider, Demo) ->
	{ok, #skr_auth_reqtoken{url=URL} = RT} = skyraid:authenticate(Provider),
	launch_user_authentication(URL),
	RTP = ask_for_pincode(RT),
	{ok, AT} = skyraid:authenticate(RTP),
	case Demo of
		account -> account(AT);
		files -> files(AT);
		write -> write(AT);
		read -> read(AT)
	end.

account(AT) ->
	{ok, Account} = skyraid_storage:account_info(AT),
	Account.

files(AT) ->
	{ok, Files} = skyraid_storage:file_list(AT),
	Files.

write(AT) ->
	ok = skyraid_storage:write_file(AT, "testing.txt", <<"hello my friend">>).

read(AT) ->
	{ok, File} = skyraid_storage:read_file(AT, "master_slave.erl"),
	File.

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

ask_for_pincode(#skr_auth_reqtoken{}=RT) ->
	PinCode = string:strip(io:get_line("Enter pin code:"), both, $\n),
	RT#skr_auth_reqtoken{verifier=PinCode}.
