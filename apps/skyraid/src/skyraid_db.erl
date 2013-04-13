-module(skyraid_db).

-include("skyraid.hrl").

-define(db, skyraid_db_ets).
%%-define(db, skyraid_db_mnesia).

-export([init/0, create_user/1, create_storage_provider/1, get_users/0, get_user/1]).
-export([create_account/1, get_account/1, get_accounts/0]).

init() ->
	?db:init().

get_users() ->
	?db:get_users().

get_user(Username) ->
	?db:get_user(Username).

create_user(User) ->
	?db:create_user(User).

create_account(Account) ->
	?db:create_account(Account).

get_account(AccountID) ->
	?db:create_account(AccountID).

get_accounts() ->
	?db:get_accounts().

create_storage_provider(Provider) ->
	?db:create_storage_provider(Provider).	