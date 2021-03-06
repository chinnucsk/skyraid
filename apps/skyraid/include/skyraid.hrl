-ifndef(__SKYRAID_HRL_INCLUDED).
-define(__SKYRAID_HRL_INCLUDED, true).

-type session_ref() :: any().
-type file_ref() :: any().

-define(DEBUG(Msg), erlang:display(Msg)).
-define(INFO(Msg), lager:info(Msg)).


-record(skr_user, {
	id = erlang:ref_to_list(make_ref()) :: list(),
	username :: binary(),
	password :: binary(),
	display_name :: binary(),
	email :: binary()
}).
-type skr_user() :: #skr_user{}.

-record(skr_session_info, {
	timestamp :: term(),
	user_id :: list(),
	user :: skr_user(),
	accounts = [] :: [skr_account()]
}).
-type skr_session_info() :: #skr_session_info{}.


-record(skr_quota_info, {
	shared :: number(),
	quota :: number(),
	normal :: number()
}).
-type skr_quota_info() :: #skr_quota_info{}.

-record(skr_account, {
	id :: term(), %% The internal id of this account
	provider :: atom(),
	ext_id :: binary(), %% The external id of this account(id at the provider)
	user_id :: term(),
	display_name :: binary(),
	country :: binary(),
	email :: binary(),
	storage_id :: atom(),
	authentication :: term(),
	quota_info :: skr_quota_info()
}).
-type skr_account() :: #skr_account{}.


-record(skr_storage, {
	  id :: atom(),
	  url :: string()
	 }).

-type skr_storage() :: #skr_storage{}.

-record(skr_file_ref, {
	account_id :: term(), %% The account id of this file
	ref :: term() %% The ref id of the file at the account
}).
-type skr_file_ref() :: #skr_file_ref{}.

-record(skr_file_info, {
	path :: string(),
	is_dir :: boolean(),
	size :: number(),
	rev :: string(),
	modified :: term(),
	mime_type :: string()
}).
-type skr_file_info() :: #skr_file_info{}.

%% ===================================================================
%% Authentication records
%% ===================================================================

-record(skr_auth_basic, {
	url :: string(),
	provider :: atom(),
	username :: binary(),
	password :: binary()
}).

-type skr_auth_basic() :: #skr_auth_basic{}.

-record(skr_auth_reqtoken, {
	url :: string(),
	provider :: atom(),
	token :: term(),
	verifier :: term()
}).
-type skr_auth_reqtoken() :: #skr_auth_reqtoken{}.

-record(skr_auth_acctoken, {
	provider :: atom(),
	token :: term()
}).
-type skr_auth_acctoken() :: #skr_auth_acctoken{}.

-endif. % __SKYRAID_HRL_INCLUDED
