-module(skyraid_integration_local).

-include_lib("skyraid/include/skyraid.hrl").

-behaviour(skyraid_account_provider).
-export([init/0, create_token/0, authenticate/1, logout/1, account_info/1]).

-behaviour(skyraid_file_provider).
-export([list_files/1, write_file/3, read_file/2]).

%% ====================================================================
%% API account provider
%% ====================================================================
init() ->
	{ok, []}.

create_token() ->
	{error, not_supported}.

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

account_info(#skr_auth_acctoken{token= {_UserId, _AccountId}, provider=local}) ->
	{error, not_implemented}.

%% ====================================================================
%% API file provider
%% ====================================================================
list_files(_Session) ->
	{error, not_implemented}.

write_file(Session, FileName, Content) ->
	FilePath = file(Session, FileName),
	file:write_file(FilePath, Content).

read_file(Session, FileName) ->
	{ok, S} = skyraid_user_session:info(Session),
	Path = "data/test/" ++ binary_to_list(S#skr_session_info.user#skr_user.username) ++ "/",
	FilePath = filename:join(Path, FileName),
	file:read_file(FilePath).

%% ====================================================================
%% Private functions
%% ====================================================================

file(Session, FileName) ->
	{ok, S} = skyraid_user_session:info(Session),
	Path = "data/test/" ++ binary_to_list(S#skr_session_info.user#skr_user.username) ++ "/",
	filelib:ensure_dir(Path),
	filename:join(Path, FileName).

%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

skyraid_integration_local_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			{"create token test", fun create_token_tc/0},
			{"authentication test", fun authenticate_tc/0},
			{"logout test", fun logout_tc/0},
			{"account info test", fun account_info_tc/0}

%%			{"list files test", fun list_files_tc/0},
%%			{"write file test", fun write_file_tc/0},
%%			{"read file test", fun read_file_tc/0}
		]
	}.

setup() ->
	%% Need to populate the db.
	skyraid_db_ets:init().

teardown(_) ->
	ok.

create_token_tc() ->
	{error, not_supported} = create_token().

authenticate_tc() ->
	Auth = #skr_auth_basic{username= <<"Adam">>, password= <<"test">>, provider=local},
	{ok, #skr_auth_acctoken{provider=local, token="0"}} = authenticate(Auth).

logout_tc() ->
	Auth = #skr_auth_basic{username= <<"Adam">>, password= <<"test">>, provider=local},
	{ok, #skr_auth_acctoken{provider=local, token="0"}=AT} = authenticate(Auth),
	ok = logout(AT).

account_info_tc() ->
	ok.
%%	Auth = #skr_auth_basic{username= <<"Adam">>, password= <<"test">>, provider=local},
%%	{ok, AT} = authenticate(Auth),
%%	{ok, #skr_account{user_id="0"}} = account_info(AT).
%%
-endif.