-module(skyraid_account_repo).

-export([new/1, all/0, get/1]).

new(#skr_account{}=Account) ->
	skyraid_db:create_account(Account).

all() ->
	skyraid_db:get_accounts().	

get(AccountId) ->
	skyraid_db:get_account(AccountId).
