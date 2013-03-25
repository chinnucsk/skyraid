-module(skyraid).
-include("skyraid.hrl").

-export([start/0, stop/0, register/1, login/2, login/1, logout/1]).

-type session() :: any().

start() ->
	application:start(?MODULE).

stop() ->
	application:stop(?MODULE).

-spec register(skr_user()) -> ok | {error, term()}.
register(User) ->
	skyraid_user_repo:create_user(User).

-spec login(string(), string()) -> {ok, session()} | {error, term()}.
login(Username, Password) ->
	skyraid_auth:login(Username, Password).

-spec login(string()) -> {ok, session()} | {error, term()}.	
login(Token) ->
	skyraid_auth:login(Token).

-spec logout(session()) -> ok | {error, term()}.
logout(SessionId) ->
	skyraid_auth:logout(SessionId).