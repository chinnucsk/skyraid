-module(skyraid_file).

-export([open/3, close/1, write/2, write_file/4, read/1]).

open(_Session, _FileName, _Opts) ->
	not_implemented.
	%{ok, Pid} = skyraid_fileop_sup:start_fileop(FileName, Opts).
	%link(Session, Pid),
	%{ok, Session}.

close(FileRef) ->
	skyraid_fileop:stop(FileRef).

write(FileRef, Content) ->
	skyraid_fileop:write(FileRef, Content).

write_file(Session, FileName, Content, Opts) ->
	StorageList = get_storages(Opts),
	[skyraid_storage:write_file(S, Session, FileName, Content, Opts) || S<-StorageList].

read(FileRef) ->
	skyraid_fileop:read(FileRef).

get_storages(Opts) ->
	case [X || {storage, [X]} <- Opts] of
		[S]-> S;
		[] -> {error, no_storage_list}
	end.