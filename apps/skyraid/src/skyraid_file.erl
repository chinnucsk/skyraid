-module(skyraid_file).

%% ====================================================================
%% API functions
%% ====================================================================
-export([open/3, close/1, write/2, write_file/4, read/1, read_file/3]).

open(Session, FileName, Opts) ->
	Storage = get_storage(Opts),
	{ok, Ref} = skyraid_storage:file_open(Storage, Session, FileName, Opts),
	{ok, {Storage, Ref}}.

close(_FileRef={Storage, Ref}) ->
	skyraid_storage:file_close(Storage, Ref).

write(_FileRef={Storage, Ref}, Content) ->
	skyraid_storage:file_write(Storage, Ref, Content).

write_file(Session, FileName, Content, Opts) ->
	StorageList = get_storages(Opts),
	Result = [{skyraid_storage:write_file(S, Session, FileName, Content, Opts), S} || S<-StorageList],
	validate(Result).

read(_FileRef={Storage, Ref}) ->
	skyraid_storage:file_read(Storage, Ref).

read_file(Session, FileName, Opts) ->
	Storage = get_storage(Opts),
	skyraid_storage:read_file(Storage, Session, FileName, Opts).

%%list(Session, Storage) ->
%%	skyraid_storage:file_list(Storage, Session).

%% ====================================================================
%% Internal functions
%% ====================================================================
get_storages(Opts) ->
	case [X || {storage, [X]} <- Opts] of
		[S]-> [S];
		[] -> {error, no_storage_list}
	end.

get_storage(Opts) ->
	case get_storages(Opts) of 
		[S] -> S;
		Any -> Any
	end.

validate(Result)->
	%% Enumerate result and store all errors, if empty all is fine.
	case [{R, S} || {R, S} <- Result, R /= ok] of
		[] -> ok;
		Errors -> {error, Errors}
	end.