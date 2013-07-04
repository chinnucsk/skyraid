-module(skyraid_file_provider_local).

-include("skyraid.hrl").
-include_lib("kernel/include/file.hrl").

-define(DATA_ROOT, "../../skyraid/.eunit/data/test/").

-behaviour(skyraid_file_provider).
-export([open/3, close/2, read/2, write/3, put/4, get/3, list/2]).
-export([mkdir/2, copy/3, move/3, delete/2]).

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
	FilePath = path(UserId, FileName),
	file:read_file(FilePath).

list(#skr_auth_basic{username=UserId}, Path) ->
    Dir = path(UserId, Path),
    {ok, FileNames} = file:list_dir(Dir),
    FileInfos = [fileinfo(Dir, F) || F <- FileNames],
    {ok, to_fileinfos(FileInfos)}.

mkdir(#skr_auth_basic{username=UserId}, UserPath) ->
    Path = file(UserId, UserPath),
    case filelib:is_file(Path) of
        false ->
            case filelib:ensure_dir(Path) of 
                ok ->
                    ok = file:make_dir(Path),
                    {ok, to_fileinfo(root_dir(UserId), UserPath)};
                {error, Reason} -> {error, Reason}
            end;
        true -> {error, already_exist}
    end.

copy(#skr_auth_basic{username=UserId}, Src, Dst) ->
    Source=path(UserId, Src),
    Destination=path(UserId, Dst),
    case {filelib:is_regular(Source), filelib:is_regular(Destination)} of
        {true, false} -> 
            case file:copy(Source, Destination) of 
                {ok, _Bytes} -> {ok, to_fileinfo(root_dir(UserId), Dst)};
                {error, Reason} -> {error, Reason}
            end;
        {true, true} ->
            {error, dst_exists};
        {false, _} ->
            {error, src_not_found}
    end.
    
move(Auth, Src, Dst) ->
    case copy(Auth, Src, Dst) of
        {ok, FileInfo} ->
            case delete(Auth, Src) of
                {ok, _FI} -> {ok, FileInfo};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

delete(#skr_auth_basic{username=UserId}, FileName) ->
    Path=path(UserId, FileName),
    case filelib:is_file(Path) of
        true ->
            {ok, FileInfo} = file:read_file_info(Path), 
            IsDir = filelib:is_dir(Path),
            ok = if 
                    IsDir -> file:del_dir(Path);
                    IsDir == false -> file:delete(Path)
                end,
            {ok, to_fileinfo("/"++FileName, IsDir, FileInfo)};
        false -> {error, file_not_found}
    end.

%% ====================================================================
%% Private functions
%% ====================================================================
root_dir(UserId) ->
    ?DATA_ROOT ++ UserId ++ "/".

path(UserId, FileName) ->
    root_dir(UserId) ++ FileName.

file(UserId, FileName) ->
    Path = ?DATA_ROOT ++ UserId ++ "/",
    filelib:ensure_dir(Path),
    filename:join(Path, FileName).

fileinfo(Root, Filename) ->
    Path=Root ++ Filename, 
    {ok, Info} = file:read_file_info(Path),
    IsDir = filelib:is_dir(Path),
    {"/" ++ Filename, IsDir, Info}.

to_fileinfo(Root, FileName) ->
    {Path, IsDir, FI} = fileinfo(Root, FileName),
    to_fileinfo(Path, IsDir, FI).

to_fileinfos(FileInfos) when is_list(FileInfos) ->
    [to_fileinfo(Path, IsDir, FI) || {Path, IsDir, FI} <- FileInfos].

to_fileinfo(Path, IsDir, #file_info{size=Size, mtime=Modified}) ->
    #skr_file_info {
        path=Path,
        size=Size,
        is_dir=IsDir,
        modified=Modified 
    }.
    
%% ====================================================================
%% Unit Tests
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(USER_ID, "Testing").
-define(ROOT, ?DATA_ROOT ++ ?USER_ID ++ "/").
-define(AUTH, #skr_auth_basic{username=?USER_ID}).

setup(list) ->
    File = "subfile.txt",
    Dir = "a_sub_dir",
    DirPath = ?ROOT ++ Dir,
    FilePath = DirPath ++ "/" ++ File, 
    file:make_dir(DirPath),
    file:write_file(FilePath, <<"my subfile">>),
    {Dir, File};


setup(copy) ->
    Src = "src_copy.txt",
    Dst = "dst_copy.txt", 
    PathCpySrc = ?ROOT ++ Src,
    PathCpyDst = ?ROOT ++ Dst,
    file:delete(PathCpySrc),
    file:delete(PathCpyDst),
    ok = filelib:ensure_dir(?ROOT),
    ok = file:write_file(PathCpySrc, <<"my file copy">>),
    {Src, Dst};

setup(move) ->
    Src = "mv_src.txt",
    Dst = "mv_dst.txt",
    PathMvSrc = ?ROOT ++ Src,
    PathMvDst = ?ROOT ++ Dst,
    file:delete(PathMvSrc),
    file:delete(PathMvDst),
    filelib:ensure_dir(?ROOT),
    file:write_file(PathMvSrc,<<"my file move">>),
    {Src, Dst};

setup(del_dir) ->
    Dir = "del_dir/",
    ok = filelib:ensure_dir(?ROOT ++ Dir),
    Dir.

setup(Root, del_file) ->
    Src = "del_src.txt",
    Path = Root ++ Src,
    file:delete(Path),
    ok = filelib:ensure_dir(Root),
    ok = file:write_file(Path, <<"my file to delete">>),
    Src.

list_test() ->
    {Dir, _File} = setup(list),
    {ok, [#skr_file_info{path="/" ++ Dir, is_dir=true} | _Rest ]} = list(?AUTH, "/").

list_sub_dir_test() ->
    {Dir, File} = setup(list),
    Path = "/" ++ File,
    {ok, [#skr_file_info{path=Path, is_dir=false} | _Rest ]} = list(?AUTH, Dir ++ "/").

mkdir_test() ->
    UserId=?USER_ID,
    Dir="mydir",
    {ok, #skr_file_info{path="/" ++ Dir, is_dir=true}} = mkdir(#skr_auth_basic{username=UserId}, Dir),
    ok = file:del_dir(root_dir(UserId) ++ Dir).

copy_test() ->
    {Src, Dst} = setup(copy),
    {ok, #skr_file_info{path="/" ++ Dst, is_dir=false}} = copy(?AUTH, Src, Dst).    

move_test() ->
    {Src, Dst} = setup(move),
    {ok, #skr_file_info{path="/" ++ Dst, is_dir=false}} = move(?AUTH, Src, Dst).

delete_file_test() ->
    File = setup(?ROOT, del_file),
    {ok, #skr_file_info{path="/" ++ File, is_dir=false}} = delete(?AUTH, File).

delete_dir_test() ->
    Dir = setup(del_dir),
    {ok, #skr_file_info{path="/" ++ Dir, is_dir=true}} = delete(?AUTH, Dir).

-endif.