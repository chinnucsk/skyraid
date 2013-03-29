-module(skyraid_auth).

-include("skyraid.hrl").

-export([authenticate/1, authenticate/2, logout/1]).

-type user_uid() :: binary().

-spec authenticate(string(), string()) -> {ok, user_uid()} | {error, invalid_password} | {error, invalid_username_password}.
authenticate(Username, Password) ->
	case validate(Username, Password) of
		{ok, User} ->
			skyraid_user_session_sup:start_session(User);
		Any -> Any
	end.

authenticate(_Token) ->
	ok.

logout(_SessionID) ->
	ok.

validate(Username, Password) ->
	{ok, U} = skyraid_user_repo:get_user(Username),
	case {U#skr_user.username, U#skr_user.password} of
		{Username, Password} -> 
			{ok, U};
		{Username, _} ->
			{error, invalid_password};
		{_, _} ->
			{error, invalid_username_password}
	end.