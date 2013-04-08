-module(skyraid).
-include("skyraid.hrl").

-export([
	start/0, stop/0, demo/0,
	register/1, login/2, login/1, logout/1, 
	user_info/1, 
	file_open/3, file_close/1, file_write/2, file_write/4, file_read/1, file_read/3
]).

-type session_ref() :: any().
-type file_ref() :: any().

%% ===================================================================
%% API Functions
%% ===================================================================
start() ->
	ok = ensure_started(crypto),
	ok = ensure_started(inets),
	ok = ensure_started(public_key),
	ok = ensure_started(ssl),
	application:start(?MODULE).

stop() ->
	application:stop(crypto),
	application:stop(inets),
	application:stop(public_key),
	application:stop(ssl),
	application:stop(?MODULE).

demo() ->
	start_demo().

-spec register(skr_user()) -> ok | {error, term()}.
register(#skr_user{} = User) ->
	skyraid_user_repo:new(User);

register(AccountProvider) ->
	case skyraid_auth:authenticate(AccountProvider) of
		{ok, {access_token, Token}} ->
			{ok, Account} = skyraid_storage:account_info(Token),
			ok = skyraid_account_repo:new(Account),
			login(Token);
		Any -> Any
	end.


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

%% ====================================================================
%% Internal functions
%% ====================================================================
ensure_started(App) ->
    case application:start(App) of
        ok ->ok;
        {error, {already_started, App}} ->ok
    end.

start_demo() ->
	{ok, {{url, URL}, RT}} = skyraid:register(dropbox),
	launch_user_authentication(URL),
	{ok, AT} = skyraid:register(RT),
	ok = skyraid_storage:account_info(AT).

launch_user_authentication(URL) ->
	Cmd = case os:type() of 
			{win, _Osname} -> "start ";
			{unix, _Osname} -> "epiphany "
		end,

	try open_port({spawn, Cmd ++ URL},[binary,{line, 255}]) of
		Port -> await_user(Port)
	catch
		_ -> ok
	end.

await_user(Port) ->
	receive 
		{Port, Any} -> 
			erlang:display(Any),
			await_user(Port)
	end. 


