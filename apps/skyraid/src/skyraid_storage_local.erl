-module(skyraid_storage_local).
-include("skyraid.hrl").

-export([write_file/4, read_file/3]).

write_file(Session, FileName, Content, _Opts) ->
	{ok, S} = skyraid_user_session:info(Session),
	Path = "data/test/" ++ S#skr_session_info.user#skr_user.username ++ "/",
	ok = filelib:ensure_dir(Path),
	FilePath = filename:join(Path, FileName),
	file:write_file(FilePath, Content).

read_file(Session, FileName, _Opts) ->
	{ok, S} = skyraid_user_session:info(Session),
	Path = "data/test/" ++ S#skr_session_info.user#skr_user.username ++ "/",
	FilePath = filename:join(Path, FileName),
	file:read_file(FilePath).

