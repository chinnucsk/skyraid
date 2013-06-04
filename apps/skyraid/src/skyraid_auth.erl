-module(skyraid_auth).

-include("skyraid.hrl").

-export([authenticate/1, login/1, login/2, logout/1, info/1]).


authenticate(Provider) when is_atom(Provider) ->
    case Provider of
	dropbox -> skyraid_storage_dropbox:authorize_url();
	google -> skyraid_storage_google:authorize_url();
	twitter -> skyraid_storage_twitter:authorize_url();
	_ -> invalid_storage
    end;

authenticate(#skr_auth_reqtoken{provider=Provider}=RT) ->
    case Provider of
	dropbox -> skyraid_storage_dropbox:access_token(RT);
	google -> skyraid_storage_google:access_token(RT);
	twitter -> skyraid_storage_twitter:access_token(RT);
	_ -> invalid_storage
    end.

-spec login(binary(), binary() | skr_auth_reqtoken()) ->
		   {ok, session_ref()} |
		   {error, invalid_password} |
		   {error, invalid_username_password}.
login(Username, Password) ->
    case validate(Username, Password) of
	{ok, User} ->
	    case skyraid_user_session_sup:start_session(User) of
		{ok, Pid} -> {ok, Pid};
		{error, {already_started, Pid}} -> {ok, Pid}
	    end;
	Any -> Any
    end.

login(#skr_auth_reqtoken{provider=Provider}=RT) ->
    case Provider of
	dropbox ->
	    {ok, AT} = skyraid_storage_dropbox:access_token(RT),
	    {ok, #skr_account{storage_id=ID}} =
		skyraid_storage_dropbox:account_info(AT),
	    {ok, #skr_account{user_id=UserID}} =
		skyraid_account_repo:get(ID),
	    {ok, User} = skyraid_user_repo:get_user_by_id(UserID),
	    skyraid_user_session_sup:start_session(User);
	_ -> {error, invalid_provider}
    end.

logout(SessionRef) ->
    skyraid_user_session_sup:stop_session(SessionRef).

info(SessionRef) ->
    case catch skyraid_user_session:info(SessionRef) of
	{'EXIT', {noproc, _}} -> {error, session_not_found};
	Response -> Response
    end.


validate(Username, Password) ->
    case skyraid_user_repo:get_user(Username) of
	{ok, U} ->
	    case {U#skr_user.username, U#skr_user.password} of
		{Username, Password} ->
		    {ok, U};
		{Username, _} ->
		    {error, invalid_password}
	    end;
	not_found ->
	    {error, invalid_username_password}
    end.
