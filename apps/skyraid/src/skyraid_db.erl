-module(skyraid_db).

-incluse("skyraid.hrl").

-export([init/0, create_user/1, create_storage_provider/1]).

init() ->
	skyraid_db_test:init().

get_users() ->
	skyraid_db_test:get_users().

create_user(User) ->
	skyraid_db_test:create_user(User).

create_storage_provider(Provider) ->
	skyraid_db_test:create_storage_provider(Provider).	