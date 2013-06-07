-ifndef(__SKYRAID_HRL_INCLUDED).
-define(__SKYRAID_HRL_INCLUDED, true).

-type session_ref() :: any().
-type file_ref() :: any().

-define(DEBUG(Msg), erlang:display(Msg)).
-define(INFO(Msg), lager:info(Msg)).

-record(skr_user, {
	  id = erlang:ref_to_list(make_ref()) :: reference(),
	  username :: binary(),
	  password :: binary(),
	  display_name :: binary(),
	  email :: binary(),
	  accounts = [] :: [skr_account()]
	 }).
-type skr_user() :: #skr_user{}.

-record(skr_session_info, {
	  timestamp :: term(),
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

%% ===================================================================
%% Authentication records
%% ===================================================================

-record(skr_auth_basic, {
	  url :: string(),
	  provider :: atom(),
	  username :: binary(),
	  password :: binary()
	 }).

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
