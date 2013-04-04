-module(skyraid_storage_local).
-include("skyraid.hrl").

-export([file_open/3, file_close/1, file_write/2, write_file/4, read_file/3]).

file_open(Session, FileName, _Opts) ->
	FilePath = file(Session, FileName),
	file:open(FilePath, [write]).

file_close(FileRef) ->
	file:close(FileRef).

file_write(FileRef, Content) ->
	file:write(FileRef, Content).

write_file(Session, FileName, Content, _Opts) ->
	FilePath = file(Session, FileName),
	file:write_file(FilePath, Content).

read_file(Session, FileName, _Opts) ->
	{ok, S} = skyraid_user_session:info(Session),
	Path = "data/test/" ++ S#skr_session_info.user#skr_user.username ++ "/",
	FilePath = filename:join(Path, FileName),
	file:read_file(FilePath).

file(Session, FileName) ->
	{ok, S} = skyraid_user_session:info(Session),
	Path = "data/test/" ++ S#skr_session_info.user#skr_user.username ++ "/",
	filename:join(Path, FileName).