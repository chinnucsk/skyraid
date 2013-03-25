-module(skyraid_db_test).

-include("skyraid.hrl").

-export([init/0, create_user/1, create_storage_provider/1]).

init() ->
	ets:new(users, [set, named_table]),
	ets:new(storage_provider, [set, named_table]).

get_users() ->
	ets:tab2list(users).

create_user(#skr_user{} = User) ->
	ets:insert(users, User).

create_storage_provider(#skr_storage_provider{} = Provider) ->
	ets:insert(storage_provider, Provider).