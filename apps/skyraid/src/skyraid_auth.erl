-module(skyraid_auth).

-include("skyraid.hrl").

-export([authenticate/1, authenticate/2, logout/1]).

-type user_uid() :: binary().

-spec authenticate(string(), string()) -> {ok, user_uid()} | {error, invalid_password} | {error, invalid_username_password}.
authenticate(Username, Password) ->
	case validate(Username, Password) of
		{ok, User} ->
			case skyraid_user_session_sup:start_session(User) of
				{ok, Pid} -> {ok, Pid};
				{error, {already_started, Pid}} -> {ok, Pid}
			end;
		Any -> Any
	end.

authenticate(_Token) ->
	not_implemented.

logout(SessionRef) ->
	skyraid_user_session_sup:stop_session(SessionRef).

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