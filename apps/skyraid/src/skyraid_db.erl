-module(skyraid_db).

-include("skyraid.hrl").

-define(db, skyraid_db_test).

-export([init/0, create_user/1, create_storage_provider/1, get_users/0, get_user/1]).

init() ->
	?db:init().

get_users() ->
	?db:get_users().

get_user(Username) ->
	?db:get_user(Username).

create_user(User) ->
	?db:create_user(User).

create_storage_provider(Provider) ->
	?db:create_storage_provider(Provider).	