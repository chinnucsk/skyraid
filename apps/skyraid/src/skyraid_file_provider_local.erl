-module(skyraid_file_provider_local).

-include("skyraid.hrl").

-behaviour(skyraid_file_provider).
-export([open/3, close/2, read/2, write/3, put/4, get/3, list/1]).

open(#skr_auth_basic{username=UserId}, FileName, _Opts) ->
    FilePath = file(UserId, FileName),
    file:open(FilePath, [write]).

close(#skr_auth_basic{}, #skr_file_ref{ref=FileRef}) ->
	   file:close(FileRef).

read(_, _) ->
	{error, not_implemented}.

write(#skr_auth_basic{}, #skr_file_ref{ref=FileRef}, Content) ->
	file:write(FileRef, Content).

put(#skr_auth_basic{username=UserId}, FileName, Content, _Opts) ->
    FilePath = file(UserId, FileName),
    file:write_file(FilePath, Content).

get(#skr_auth_basic{username=UserId}, FileName, _Opts) ->
	FilePath = file(UserId, FileName),
	file:read_file(FilePath).

list(#skr_auth_basic{username=_UserId}) ->
	ok.

file(UserId, FileName) ->
    Path = "data/test/" ++ UserId ++ "/",
    filelib:ensure_dir(Path),
    filename:join(Path, FileName).