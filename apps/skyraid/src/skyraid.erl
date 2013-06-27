-module(skyraid).
-include("skyraid.hrl").

-export([
	 start/0, stop/0,
	 register/1,
	 authenticate/1,
	 login/2,
	 login/1,
	 logout/1,
	 get_session/1,
	 add_account/2,
	 file_list/2,
	 file_open/4,
	 file_close/2,
	 file_write/3,
	 file_write/4,
	 file_write/5,
     file_read/2,
	 file_read/4
]).

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

-spec authenticate(atom() | skr_auth_reqtoken()) ->
			  {ok, skr_auth_reqtoken()} |
			  {ok, skr_auth_acctoken()} |
			  {error | term()}.
authenticate(Provider) when is_atom(Provider)->
    skyraid_auth:authenticate(Provider);

authenticate(#skr_auth_reqtoken{}=RT) ->
    skyraid_auth:authenticate(RT).

-spec register(skr_user() | skr_auth_reqtoken()) ->
		      ok | {error, term()}.
register(#skr_user{} = User) ->
    skyraid_user_repo:new(User);

register(#skr_auth_reqtoken{}=RT) ->
    {ok, #skr_auth_acctoken{}=AT} = skyraid_auth:authenticate(RT),
    {ok, Account} = skyraid_storage:account_info(AT),
    ok = skyraid_account_repo:new(Account).

-spec login(binary(), binary()) ->
		   {ok, session_ref()} | {error, term()}.
login(Username, Password) ->
    skyraid_auth:login(Username, Password).

-spec login(skr_auth_reqtoken()) ->
		   {ok, session_ref()} | {error, term()}.
login(#skr_auth_reqtoken{}=RT) ->
    skyraid_auth:login(RT).

-spec logout(session_ref()) -> ok | {error, term()}.
logout(SessionRef) ->
    skyraid_auth:logout(SessionRef).

-spec get_session(session_ref()) ->
			 {ok, skr_session_info()} | {error, term()}.
get_session(SessionRef) ->
    skyraid_auth:info(SessionRef).

-spec add_account(session_ref(), skr_auth_reqtoken()) ->
			 {ok, skr_session_info()} | {error, term()}.
add_account(SessionRef, #skr_auth_reqtoken{}=RT) ->
    {ok, AT} = skyraid_auth:authenticate(RT),
    {ok, RemoteAccount} = skyraid_storage:account_info(AT),
    {ok, #skr_session_info{user=#skr_user{id=UserID}}} = skyraid_user_session:info(SessionRef),
    {ok, LocalAccount} = skyraid_account_repo:new(RemoteAccount#skr_account{user_id=UserID}),
    skyraid_user_session:add_account(SessionRef, LocalAccount);

add_account(SessionRef, #skr_account{}=A) ->
    {ok, #skr_session_info{user=#skr_user{id=UserID}}} = skyraid_user_session:info(SessionRef),
    {ok, LocalAccount} = skyraid_account_repo:new(A#skr_account{user_id=UserID}),
    skyraid_user_session:add_account(SessionRef, LocalAccount).

file_list(SessionRef, AccountID) ->
    skyraid_file:file_list(SessionRef, AccountID).

-spec file_open(session_ref(), term(), string(), list()) -> {ok, file_ref()}.
file_open(SessionRef, AccountID, FileName, Opts) ->
    skyraid_file:open(SessionRef, AccountID, FileName, Opts).

-spec file_close(session_ref(), file_ref()) -> ok | {error, term()}.
file_close(SessionRef, FileRef) ->
    skyraid_file:close(SessionRef, FileRef).

-spec file_write(session_ref(), file_ref(), binary()) -> ok | {error, term()}.
file_write(SessionRef, FileRef, Content) ->
    skyraid_file:write(SessionRef, FileRef, Content).

-spec file_write(session_ref(), string(), binary(), list()) ->
			ok | {error, term()}.
file_write(SessionRef, FileName, Content, Opts) ->
    skyraid_file:write_file(SessionRef, FileName, Content, Opts).

-spec file_write(session_ref(), atom(), string(), binary(), list()) ->
			ok | {error, term()}.
file_write(SessionRef, AccountID, FileName, Content, Opts) ->
    skyraid_file:write_file(SessionRef, AccountID, FileName, Content, Opts).

-spec file_read(session_ref(), file_ref()) -> {ok, binary()} | {error, term()}.
file_read(SessionRef, FileRef) ->
    skyraid_file:read(SessionRef, FileRef).

-spec file_read(session_ref(), string(), string(), list()) -> {ok, binary()} | {error, term()}.
file_read(SessionRef, AccountID, FileName, Opts) ->
    skyraid_file:read_file(SessionRef, AccountID, FileName, Opts).

%% ====================================================================
%% Internal functions
%% ====================================================================
ensure_started(App) ->
    case application:start(App) of
        ok ->ok;
        {error, {already_started, App}} ->ok
    end.
