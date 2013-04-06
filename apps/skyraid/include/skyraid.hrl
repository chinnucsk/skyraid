
-record(skr_user, 
{
	uid = erlang:ref_to_list(make_ref()) :: reference(),
	username :: binary(),
	password :: binary(),
	display_name :: binary(),
	email :: binary(),
	storage :: [skr_storage()]
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
	id :: term(),
	storage :: atom(),
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