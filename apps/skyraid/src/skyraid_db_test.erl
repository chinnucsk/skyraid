-module(skyraid_db_test).

-include("skyraid.hrl").

-export([init/0, create_user/1, create_storage/1, get_users/0, get_user/1]).

init() ->
	users = ets:new(users, [set, {keypos, 3}, named_table, public]),
	storages = ets:new(storages, [set, named_table, public]),
	insert_test_data(),
	ok.

get_users() ->
	case ets:tab2list(users) of
		Users -> {ok, Users}
	end.


get_user(Username)->
	case ets:lookup(users, Username) of
		[] -> not_found;
		[User] -> {ok, User}
	end.


create_user(#skr_user{} = User) ->
	case ets:insert(users, User) of 
		true-> ok; 
		Any-> {error, Any}
	end.

create_storage(#skr_storage{} = Storage) ->
	ets:insert(storages, Storage).

insert_test_data() ->
	Adam = #skr_user{username = <<"Adam">>, 
			  password = <<"test">>, 
			  display_name = <<"AdamDisplay">>, 
			  email = <<"adam@gmail.com">>},

	Eva = #skr_user{username = <<"Eva">>, 
	  password = <<"test">>, 
	  display_name = <<"EvaDisplay">>, 
	  email = <<"adam@gmail.com">>},

	ets:insert(users, Adam),
	ets:insert(users, Eva).


