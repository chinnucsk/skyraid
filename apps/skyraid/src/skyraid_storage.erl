-module(skyraid_storage).

-export([authorize_url/1, access_token/2, account_info/2, file_write/5]).

authorize_url(dropbox) ->
	skyraid_storage_dropbox:authorize_url().

access_token(dropbox, RequestToken) ->
	skyraid_storage_dropbox:access_token(RequestToken).

account_info(dropbox, AccessToken) ->
	skyraid_storage_dropbox:account_info(AccessToken).

file_write(local, Session, FileName, Content, Opts) ->
	skyraid_storage_local:file_write(Session, FileName, Content, Opts).

