-module(skyraid).
-include("skyraid.hrl").

-export([
	start/0, stop/0, 
	register/1, login/2, login/1, logout/1, 
	user_info/1, 
	file_open/3, file_close/1, file_write/2, file_write/4, file_read/1, file_read/3
]).

-type session_ref() :: any().
-type file_ref() :: any().


start() ->
	application:start(?MODULE).

stop() ->
	application:stop(?MODULE).

-spec register(skr_user()) -> ok | {error, term()}.
register(User) ->
	skyraid_user_repo:new(User).

-spec login(string(), string()) -> {ok, session_ref()} | {error, term()}.
login(Username, Password) ->
	skyraid_auth:authenticate(Username, Password).

-spec login(string()) -> {ok, session_ref()} | {error, term()}.	
login(Token) ->
	skyraid_auth:authenticate(Token).

-spec logout(session_ref()) -> ok | {error, term()}.
logout(SessionRef) ->
	skyraid_auth:logout(SessionRef).

-spec user_info(session_ref()) -> {ok, skr_user()} | {error, term()}.
user_info(SessionRef) ->
	skyraid_user:info(SessionRef).

-spec file_open(session_ref(), string(), list()) -> {ok, file_ref()}.
file_open(SessionRef, FileName, Opts) ->
	skyraid_file:open(SessionRef, FileName, Opts).

-spec file_close(file_ref()) -> ok | {error, term()}.
file_close(FileRef) ->
	skyraid_file:close(FileRef).

-spec file_write(file_ref(), binary()) -> ok | {error, term()}.
file_write(FileRef, Content) ->
	skyraid_file:write(FileRef, Content).

-spec file_write(session_ref(), string(), binary(), list()) -> ok | {error, term()}.
file_write(SessionRef, FileName, Content, Opts) ->
	skyraid_file:write_file(SessionRef, FileName, Content, Opts).

-spec file_read(file_ref()) -> {ok, binary()} | {error, term()}.
file_read(FileRef) ->
	skyraid_file:read(FileRef).

-spec file_read(session_ref(), string(), list()) -> ok | {error, term()}.
file_read(SessionRef, FileName, Opts) ->
	skyraid_file:read_file(SessionRef, FileName, Opts).
