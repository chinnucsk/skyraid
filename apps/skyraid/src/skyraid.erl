-module(skyraid).
-include("skyraid.hrl").

-export([
	start/0, stop/0, 
	register/1, login/2, login/1, logout/1, 
	user_info/1, 
	file_open/2, file_close/1, file_write/2, file_read/1
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
logout(SessionId) ->
	skyraid_auth:logout(SessionId).

-spec user_info(session_ref()) -> {ok, skr_user()} | {error, term()}.
user_info(SessionRef) ->
	skyraid_user:info(SessionRef).

-spec file_open(string(), list()) -> {ok, file_ref()}.
file_open(_Name, _Opts) ->
	ok.	

-spec file_close(file_ref()) -> ok | {error, term()}.
file_close(_File) ->
	ok.

-spec file_write(file_ref(), binary()) -> ok | {error, term()}.
file_write(_File, _Content) ->
	ok.

-spec file_read(file_ref()) -> {ok, binary()} | {error, term()}.
file_read(_File) ->
	ok.
