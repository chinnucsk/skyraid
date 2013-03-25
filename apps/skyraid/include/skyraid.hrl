
-record(skr_user, 
{
	uid :: string(),
	username :: string(),
	password :: string(),
	display_name :: string(),
	email :: string(),
	storage :: [skr_storage()]
}).
-type skr_user() :: #skr_user{}.


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