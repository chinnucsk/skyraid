-module(skyraid_db_ets).

-include("skyraid.hrl").

-export([init/0, create_user/1, create_storage/1, get_users/0, get_user/1, get_user_by_id/1]).
-export([create_account/1, get_account/1, get_accounts/0]).

init() ->
	users = ets:new(users, [set, {keypos, 3}, named_table, public]),
	accounts = ets:new(accounts, [set, named_table, public]),
	storages = ets:new(storages, [set, named_table, public]),
	insert_test_data(),
	ok.

get_users() ->
	case ets:tab2list(users) of
		Users -> {ok, Users}
	end.


get_user(Username) ->
	case ets:lookup(users, Username) of
		[] -> not_found;
		[User] -> {ok, User}
	end.

get_user_by_id(ID) ->
	case ets:lookup_element(users, ID, #skr_user.id) of
		[] -> not_found;
		[User] -> {ok, User}
	end.


create_user(#skr_user{} = User) ->
	case ets:insert(users, User) of 
		true-> {ok, User}; 
		Any-> {error, Any}
	end.

get_account(AccountID) ->
	case ets:lookup(accounts, AccountID) of
		[Account] -> {ok, Account};
		[] -> not_found
	end.

get_accounts() ->
	ets:tab2list(accounts).

create_account(Account) ->
	case ets:insert(accounts, Account) of 
		true-> {ok, Account}; 
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

