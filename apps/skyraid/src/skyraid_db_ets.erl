-module(skyraid_db_ets).

-include("skyraid.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/0,
	 close/0,
	 create_user/1,
	 create_storage/1,
	 get_users/0,
	 get_user/1,
	 get_user_by_id/1]).
-export([create_account/1,
	 get_account/1,
	 get_accounts/1,
	 get_all_accounts/0]).

init() ->
    users = ets:new(users, [set, {keypos, 3}, named_table, public]),
    accounts = ets:new(accounts, [set, {keypos, 2}, named_table, public]),
    storages = ets:new(storages, [set, named_table, public]),
    insert_test_data(),
    ok.

close() ->
    ets:delete(users),
    ets:delete(accounts),
    ets:delete(storages).

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
    case ets:match_object(users, #skr_user{id=ID, _='_'}) of
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
		[] -> not_found;
		[Account] -> {ok, Account}
    end.

get_accounts(UserID) ->
    case ets:match_object(accounts, #skr_account{user_id=UserID, _='_'})  of
	Accounts -> {ok, Accounts}
    end.

get_all_accounts() ->
    case ets:tab2list(accounts) of
		Accounts -> {ok, Accounts}
    end.

create_account(Account) ->
    case ets:insert(accounts, Account) of
		true-> {ok, Account};
		Any-> {error, Any}
    end.

create_storage(#skr_storage{} = Storage) ->
    ets:insert(storages, Storage).

insert_test_data() ->
    Adam = #skr_user{
	      id="0",
	      username = <<"Adam">>,
	      password = <<"test">>,
	      display_name = <<"AdamDisplay">>,
	      email = <<"adam@gmail.com">>},

    Eva = #skr_user{
	     id="1",
	     username = <<"Eva">>,
	     password = <<"test">>,
	     display_name = <<"EvaDisplay">>,
	     email = <<"adam@gmail.com">>},

    AdamAccount1 = #skr_account {
		      id="0",
		      user_id="0",
		      provider=local,
		      display_name= <<"AdamAccount1">>,
		      authentication = #skr_auth_basic {
					  provider=local,
					  username="User",
					  password="test"
					 }
		     },

    EvaAccount1 = #skr_account {
		      id="1",
		      user_id="1",
		      display_name= <<"EvaAccount1">>,
		      provider=local,
		      authentication = #skr_auth_basic {
					  provider=local,
					  username="User",
					  password="test"
					 }
		     },		     

    ets:insert(users, Adam),
    ets:insert(users, Eva),

    ets:insert(accounts, AdamAccount1),
    ets:insert(accounts, EvaAccount1).


%% ====================================================================
%% Unit Tests
%% ====================================================================
-ifdef(TEST).
skyraid_db_ets_test_() ->
    {setup, fun setup/0, fun teardown/1,
    	[
      		{"get_user_tc", fun get_user_tc/0},
      		{"get_accounts_tc", fun get_accounts_tc/0},
      		{"get_all_accounts_tc", fun get_all_accounts_tc/0}
     	]
    }.

setup() -> init().
teardown(_) -> close().

get_user_tc() ->
    {ok, #skr_user{username = <<"Adam">>}} = get_user(<<"Adam">>).

get_accounts_tc() ->
    {ok, [#skr_account{user_id = "0"}]} = get_accounts("0").

get_account_tc() ->
	{ok, [#skr_account{id="0"}|_]} = get_account("0").

get_all_accounts_tc() ->
    {ok, [#skr_account{} | _Rest]} = get_all_accounts().

-endif.
