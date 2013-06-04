-type session_ref() :: any().
-type file_ref() :: any().

-define(DEBUG(Msg), erlang:display(Msg)).
-define(INFO(Msg), lager:info(Msg)).

-record(skr_user, 
{
	id = erlang:ref_to_list(make_ref()) :: reference(),
	username :: binary(),
	password :: binary(),
	display_name :: binary(),
	email :: binary(),
	accounts = [] :: [skr_account()]
}).
-type skr_user() :: #skr_user{}.

-record(skr_session_info, 
{
	timestamp :: term(),
	user :: skr_user(),
	accounts = [] :: [skr_account()]
}).
-type skr_session_info() :: #skr_session_info{}.


-record(skr_quota_info,
{
	shared :: number(),	%% Shared number of bytes
	quota :: number(),	%% Total amount of bytes available for this account
	normal :: number() 	%% 
}).
-type skr_quota_info() :: #skr_quota_info{}.

-record(skr_account, 
{
	id :: term(), 					%% Internal id of this account
	ext_id :: binary(), 			%% External id of this account(id at the provider)
	user_id :: term(), 				%% Id of the user owning the account
	display_name :: binary(),		%% Name of the account
	country :: binary(),			%% Country of the account
	email :: binary(),				%% Email of the account
	provider :: atom(),				%% Provider of this account(local, dropbox, ...)
	authentication :: term(),		%% Authentication for this account
	quota_info :: skr_quota_info()	%% The quota for this account
}).
-type skr_account() :: #skr_account{}.


-record(skr_storage, 
{
	id :: atom(),
	url :: string()
}).
-type skr_storage() :: #skr_storage{}.

-record(skr_file_info, 
{
	size :: number(),
	rev :: string(),
	path :: string(),
	modified :: term(),
	mime_type :: string()
}).
-type skr_file_info() :: #skr_file_info{}.

%% ===================================================================
%% Authentication records
%% ===================================================================

-record(skr_auth_basic,
{
	url :: string(),
	provider :: atom(),
	username :: binary(),
	password :: binary()	
}).
-type skr_auth_basic() :: skr_auth_basic().

-record(skr_auth_reqtoken, 
{
	url :: string(),
	provider :: atom(),
	token :: term(),
	verifier :: term()
}).
-type skr_auth_reqtoken() :: #skr_auth_reqtoken{}.

-record(skr_auth_acctoken, 
{
	provider :: atom(),
	token :: term()
}).
-type skr_auth_acctoken() :: #skr_auth_acctoken{}.