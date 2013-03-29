-module(skyraid_storage).

-export([authorize_url/1, access_token/2, account_info/2]).

authorize_url(dropbox) ->
	skyraid_storage_dropbox:authorize_url().

access_token(dropbox, RequestToken) ->
	skyraid_storage_dropbox:access_token(RequestToken).

account_info(dropbox, AccessToken) ->
	skyraid_storage_dropbox:account_info(AccessToken).

