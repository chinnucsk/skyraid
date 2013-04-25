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
			?T(write_chunked_normal),
			?T(write_file_normal),
			?T(read_file_normal)
		] 
	}.

setup() ->
	skyraid:start().

teardown(_Any) ->
	skyraid:stop().

register_normal() ->
	User = #skr_user{username = <<"Apa">>, 
				  password = <<"test">>, 
				  display_name = <<"ApaDisplay">>, 
				  email = <<"adam@gmail.com">>},
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

%%add_account() ->
%%	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
%%	{ok, #skr_auth_reqtoken{token=Token}} = skyraid:authenticate(dropbox),
%%	{ok, Session} = skyraid:add_account(Session, Token).

write_chunked_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	{ok, FileRef} = skyraid:file_open(Session, "Chunked.txt", [{storage, [local]}]),
	skyraid:file_write(FileRef, <<"Rad1\n">>),
	skyraid:file_write(FileRef, <<"Rad2\n">>),
	skyraid:file_write(FileRef, <<"Rad3\n">>),
	skyraid:file_close(FileRef),
	{ok, <<"Rad1\nRad2\nRad3\n">>} = skyraid:file_read(Session, "Chunked.txt", [{storage, [local]}]).

write_file_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	?assertEqual(ok, skyraid:file_write(Session, "myfile.txt", <<"hello world">>, [{storage, [local]}])).

read_file_normal() ->
	{ok, Session} = skyraid:login(<<"Adam">>, <<"test">>),
	ok = skyraid:file_write(Session, "ReadFile.txt", <<"hello world">>, [{storage, [local]}]),
	?assertEqual({ok, <<"hello world">>}, skyraid:file_read(Session, "ReadFile.txt", [{storage, [local]}])).