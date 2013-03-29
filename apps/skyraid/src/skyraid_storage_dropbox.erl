-module(skyraid_storage_dropbox).

-define(key, "99s6c0nh6oeez3m").
-define(secret, "obv1gjb2aoq1zg6").
-define(auth_url, "https://www.dropbox.com/1/oauth/authorize?oauth_token=").

-export([authorize_url/0, access_token/1, account_info/1]).

authorize_url() ->
	T = [{"oauth_token_secret", _TokenSecret}, {"oauth_token", Token}] = request_token(),
	{{url, ?auth_url ++ Token}, {request_token, T}}.

access_token([{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}]) ->
	dropbox:access_token(?key, ?secret, Token, TokenSecret).

account_info([{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}, {"uid", _Uid}]) ->
	dropbox:account_info(?key, ?secret, Token, TokenSecret).

%% ====================================================================
%% Internal functions
%% ====================================================================

request_token() ->
	dropbox:request_token(?key, ?secret).

	

