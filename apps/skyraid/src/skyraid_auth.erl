-module(skyraid_auth).

-include("skyraid.hrl").

-export([create_token/1, authenticate/1, login/1, login/2, logout/1, info/1]).


create_token(Provider) when is_atom(Provider) ->
	{ok, AccountModule} = skyraid_context:get_account_provider(Provider),
	AccountModule:create_token().

authenticate(#skr_auth_reqtoken{provider=Provider}=RT) ->
	{ok, AccountModule} = skyraid_context:get_account_provider(Provider),
	AccountModule:authenticate(RT).

-spec login(binary(), binary() | skr_auth_reqtoken()) ->
		   {ok, session_ref()} |
		   {error, invalid_password} |
		   {error, invalid_username_password}.
login(Username, Password) ->
	{ok, AccountModule} = skyraid_context:get_account_provider(local),
	case AccountModule:authenticate(#skr_auth_basic{username=Username, password=Password, provider=local}) of
		{ok, _} ->
			{ok, User} = skyraid_user_repo:get_user(Username),
			case skyraid_user_session_sup:start_session(User) of
				{ok, Pid} -> {ok, Pid};
				{error, {already_started, Pid}} -> {ok, Pid}
	    	end;
	    {error, Reason} -> {error, Reason}
	end.

login(#skr_auth_reqtoken{provider=Provider}=RT) when is_atom(Provider) ->
	{ok, AccountModule} = skyraid_context:get_account_provider(Provider),
	{ok, AT} = AccountModule:authenticate(RT),
	{ok, #skr_account{id=ID}} = AccountModule:account_info(AT),
	{ok, #skr_account{user_id=UserID}} = skyraid_account_repo:get(ID),
	{ok, User} = skyraid_user_repo:get_user_by_id(UserID),
	skyraid_user_session_sup:start_session(User).

logout(SessionRef) ->
    skyraid_user_session_sup:stop_session(SessionRef).

info(SessionRef) ->
    case catch skyraid_user_session:info(SessionRef) of
		{'EXIT', {noproc, _}} -> {error, session_not_found};
		Response -> Response
    end.