-module(skyraid_account_provider_local).

-include("skyraid.hrl").

-behaviour(skyraid_account_provider).

-export([init/0, create_token/0, authenticate/1, logout/1, account_info/1, account_info/2]).

%% ====================================================================
%% API account provider
%% ====================================================================
init() ->
	{ok, []}.

create_token() ->
	%% Only used for tests
	{ok, #skr_auth_reqtoken{url="www.test.com", provider=local}}.

authenticate(#skr_auth_reqtoken{provider=local}) ->
	%% Only used for test
	{ok, #skr_auth_acctoken{provider=local, token="0"}};

authenticate(#skr_auth_basic{username=Username, password=Password, provider=local}) ->
	case skyraid_user_repo:get_user(Username) of
	 	{ok, U} ->
			case {U#skr_user.username, U#skr_user.password} of
				{Username, Password} -> 
					{ok, #skr_auth_acctoken{provider=local, token=U#skr_user.id}};
				{Username, _} ->
					{error, invalid_password}
			end;
		not_found ->
			{error, invalid_username_password}
	end.

logout(_Session) ->
	ok.

account_info(#skr_auth_acctoken{token=UserID}=AT) ->
	{ok, #skr_account{id=make_ref(), user_id=UserID, provider=local, authentication=AT}}.

account_info(#skr_auth_acctoken{}, AccountID) ->
	skyraid_account_repo:get_account(AccountID).

%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

skyraid_local_file_provider_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			{"create token test", fun create_token_tc/0},
			{"authentication test", fun authenticate_tc/0},
			{"logout test", fun logout_tc/0},
			{"account info test", fun account_info_tc/0}
		]
	}.

setup() ->
	%% Need to populate the db.
	skyraid:start().

teardown(_) ->
	skyraid:stop().

create_token_tc() ->
	{ok, #skr_auth_reqtoken{}} = create_token().

authenticate_tc() ->
	Auth = #skr_auth_basic{username= <<"Adam">>, password= <<"test">>, provider=local},
	{ok, #skr_auth_acctoken{provider=local, token="0"}} = authenticate(Auth).

logout_tc() ->
	Auth = #skr_auth_basic{username= <<"Adam">>, password= <<"test">>, provider=local},
	{ok, #skr_auth_acctoken{provider=local, token="0"}=AT} = authenticate(Auth),
	ok = logout(AT).

account_info_tc() ->
	Auth = #skr_auth_basic{username= <<"Adam">>, password= <<"test">>, provider=local},
	{ok, AT} = authenticate(Auth),
	{ok, #skr_account{user_id="0"}} = account_info(AT, "0").

-endif.