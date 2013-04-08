
-record(skr_user, 
{
	uid = erlang:ref_to_list(make_ref()) :: reference(),
	username :: binary(),
	password :: binary(),
	display_name :: binary(),
	email :: binary(),
	accounts :: [skr_account()]
}).
-type skr_user() :: #skr_user{}.

-record(skr_session_info, 
{
	timestamp :: term(),
	user :: skr_user()
}).
-type skr_session_info() :: #skr_session_info{}.


-record(skr_quota_info,
{
	shared :: number(),
	quota :: number(),
	normal :: number()
}).
-type skr_quota_info() :: #skr_quota_info{}.

-record(skr_account, 
{
	uid :: term(), %% The internal id of this account
	ext_id :: binary(), %% The external id of this account(id at the provider)
	user_id :: term(),
	display_name :: binary(),
	country :: binary(),
	email :: binary(),
	storage_id :: atom(),
	token :: string(),
	quota_info :: skr_quota_info()	
}).
-type skr_account() :: #skr_account{}.


-record(skr_storage, 
{
	id :: atom(),
	url :: string()
}).
-type skr_storage() :: #skr_storage{}.