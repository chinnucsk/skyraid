-module(skyraid_storage_twitter).

-include("skyraid.hrl").

-define(key, "6b8S6hN6SK2YvYySTfpVg").
-define(secret, "1sXRDByUnuTVw3kD6zWrVLvpdqvJHuH0k5QJ91jt8").
-define(auth_url, "https://api.twitter.com/oauth/authorize").
-define(request_token_url, "https://api.twitter.com/oauth/request_token").
-define(access_token_url, "https://api.twitter.com/oauth/access_token").

-export([authorize_url/0, access_token/1]).

authorize_url() ->
    T = [{"oauth_token", Token},
	 {"oauth_token_secret", _TokenSecret}, _] = request_token(),
    URL = oauth:uri(?auth_url, [{"oauth_token", Token}]),
    {ok, #skr_auth_reqtoken{provider=twitter, url=URL, token=T}}.

access_token(#skr_auth_reqtoken{provider=twitter,
				token=[{"oauth_token", Token},
				       {"oauth_token_secret",
					TokenSecret}, _],
				verifier=Verifier}) ->
    T = access_token(?key, ?secret, Token, TokenSecret, Verifier),
    {ok, #skr_auth_acctoken{provider=twitter, token=T}}.

%% --------------------------------------------------------------------
%% Twitter functions
%% --------------------------------------------------------------------
request_token() ->
    request_token(?key, ?secret).

request_token(Key, Secret) ->
    {ok, RequestToken} = oauth:post(?request_token_url,
				    [],
				    {Key, Secret, hmac_sha1}),
    oauth:params_decode(RequestToken).

%% authorize(Key, Secret, Token, TokenSecret, Callback) ->
%%  {ok, Authorize} = oauth:get("https://www.twitter.com/1/oauth/authorize", [{"oauth_callback", Callback}], {Key, Secret, hmac_sha1}, Token, TokenSecret),
%%  oauth:params_decode(Authorize).

access_token(Key, Secret, Token, TokenSecret, Verifier) ->
    erlang:display({Key, Secret, Token, TokenSecret, Verifier}),
    {ok, AccessToken} = oauth:post(?access_token_url,
				   [{"oauth_verifier", Verifier}],
				   {Key, Secret, hmac_sha1},
				   Token,
				   TokenSecret),
    oauth:params_decode(AccessToken).
