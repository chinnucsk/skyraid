-module(skyraid_test).

-include("skyraid.hrl").
%-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(T(TestName), {atom_to_list(TestName), fun TestName/0}).

all_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			?T(register_normal),
			?T(login_normal),
			?T(login_invalid_password),
			?T(login_invalid_username_password),
			?T(logout_normal),
			?T(get_session_normal),
			?T(get_session_not_found),
			?T(write_chunked_normal),
			?T(write_file_normal),
			?T(read_file_normal),
			?T(add_account_normal),
			?T(file_list_normal)
		] 
	}.

setup() ->
	skyraid:start().

teardown(_Any) ->
	skyraid:stop().

register_normal() ->
	User = #skr_user{
			username = <<"Apa">>, 
			password = <<"test">>, 
			display_name = <<"ApaDisplay">>, 
			email = <<"adam@gmail.com">>
		},
	?assertEqual({ok, User}, skyraid:register(User)).

login_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	{ok, I} = skyraid_user_session:info(Session),
	?assertEqual(I#skr_session_info.user#skr_user.username, <<"Adam">>).

login_invalid_password() ->
	?assertEqual({error, invalid_password}, skyraid:login(<<"Adam">>, "sdfdsfdsf")).

login_invalid_username_password() ->
	?assertEqual({error, invalid_username_password}, skyraid:login("sdasd", "sdfdf")).	

logout_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	{ok, _Info} = skyraid_user_session:info(Session),
	ok = skyraid:logout(Session),
	?assertException(exit, _, skyraid_user_session:info(Session)).

get_session_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	{ok, #skr_session_info{}} = skyraid:get_session(Session).

get_session_not_found() ->
	Session = list_to_pid("<0.1.1>"),
	{error, session_not_found} = skyraid:get_session(Session).

write_chunked_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	AccountID = "0",
	{ok, FileRef} = skyraid:file_open(Session, AccountID, "Chunked.txt", []),
	skyraid:file_write(Session, FileRef, <<"Rad1\n">>),
	skyraid:file_write(Session, FileRef, <<"Rad2\n">>),
	skyraid:file_write(Session, FileRef, <<"Rad3\n">>),
	skyraid:file_close(Session, FileRef),
	{ok, <<"Rad1\nRad2\nRad3\n">>} = skyraid:file_read(Session, AccountID, "Chunked.txt", []).

write_file_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	AccountID = "0",
	?assertEqual(ok, skyraid:file_write(Session, AccountID, "myfile.txt", <<"hello world">>, [])).

read_file_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	AccountID = "0",
	ok = skyraid:file_write(Session, AccountID, "ReadFile.txt", <<"hello world">>, []),
	?assertEqual({ok, <<"hello world">>}, skyraid:file_read(Session, AccountID, "ReadFile.txt", [])).

add_account_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	{ok, Token} = skyraid:create_token(local),
	AccountName = <<"NewAccount">>,
	{ok, #skr_session_info{accounts=[#skr_account{display_name=AccountName} | _Rest]}} = skyraid:add_account(Session, AccountName, Token).

file_list_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	AccountID = "0",
	Dir = "/",
	{ok, [#skr_file_info{}|_Rest]} = skyraid:file_list(Session, AccountID, Dir).