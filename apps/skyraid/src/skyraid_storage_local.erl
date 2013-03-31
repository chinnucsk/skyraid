-module(skyraid_storage_local).
-include("skyraid.hrl").

-export([file_write/4]).

write_file(Session, FileName, Content, _Opts) ->
	{ok, S} = skyraid_user_session:info(Session),
	Path = filename:join("../data/test", S#skr_session_info.user#skr_user.username, FileName),
	file:write_file(Path, Content).


