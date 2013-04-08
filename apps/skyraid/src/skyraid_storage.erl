-module(skyraid_storage).

-export([authorize_url/1, access_token/2, account_info/1, file_open/4, file_close/2, file_write/3, write_file/5, read_file/4]).

authorize_url(dropbox) ->
	skyraid_storage_dropbox:authorize_url().

access_token(dropbox, RequestToken) ->
	skyraid_storage_dropbox:access_token(RequestToken).

account_info({dropbox, AccessToken}) ->
	skyraid_storage_dropbox:account_info(AccessToken).

file_open(local, Session, FileName, Opts) ->
	skyraid_storage_local:file_open(Session, FileName, Opts).

file_close(local, FileRef) ->
	skyraid_storage_local:file_close(FileRef).

file_write(local, FileRef, Content) ->
	skyraid_storage_local:file_write(FileRef, Content).

write_file(local, Session, FileName, Content, Opts) ->
	skyraid_storage_local:write_file(Session, FileName, Content, Opts).

read_file(local, Session, FileName, Opts) ->
	skyraid_storage_local:read_file(Session, FileName, Opts).


