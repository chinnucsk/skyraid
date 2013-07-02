-module(skyraid_account_repo).

-include("skyraid.hrl").

-export([new/1, get_all_accounts/0, get_accounts/1, get_account/1]).

new(#skr_account{}=Account) ->
    skyraid_db:create_account(Account).

get_all_accounts() ->
    skyraid_db:get_all_accounts().

get_accounts(UserId) ->
    skyraid_db:get_accounts(UserId).

get_account(AccountId) ->
    skyraid_db:get_account(AccountId).
