-module(skyraid_storage).

-include("skyraid.hrl").

-export([authorize_url/1,
	 access_token/1,
	 account_info/1,
	 file_list/1,
	 file_open/4,
	 file_close/2,
	 file_write/3,
	 write_file/3,
	 write_file/5,
	 read_file/2,
	 read_file/4]).

authorize_url(dropbox) ->
    skyraid_storage_dropbox:authorize_url().

access_token(#skr_auth_reqtoken{provider=dropbox}=RT) ->
    skyraid_storage_dropbox:access_token(RT).

account_info(#skr_auth_acctoken{provider=dropbox}=AT) ->
    skyraid_storage_dropbox:account_info(AT).

file_list(#skr_auth_basic{provider=ftp}=AB) ->
    skyraid_storage_ftp:list_files(AB);

file_list(#skr_auth_acctoken{provider=dropbox}=AT) ->
    skyraid_storage_dropbox:list_files(AT).

write_file(#skr_auth_acctoken{provider=dropbox}=
	       AT, FileName, Content) ->
    skyraid_storage_dropbox:write_file(AT, FileName, Content).

read_file(#skr_auth_acctoken{provider=dropbox}=AT, FileName) ->
    skyraid_storage_dropbox:read_file(AT, FileName).

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
