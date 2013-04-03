
-record(skr_user, 
{
	uid = erlang:ref_to_list(make_ref()) :: reference(),
	username :: string(),
	password :: string(),
	display_name :: string(),
	email :: string(),
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

-record(skr_storage, 
{
	storage_provider_id :: atom(),
	token :: string(),
	quota_info :: skr_quota_info()	
}).
-type skr_storage() :: #skr_storage{}.


-record(skr_storage_provider, 
{
	id :: atom(),
	url :: string()
}).
-type skr_storage_provider() :: #skr_storage_provider{}.