-module(skyraid_storage_facebook).

-include("skyraid.hrl").

-export([authorize_url/0, access_token/1, account_info/1]).

-define(provider, facebook).
-define(key, "468839249851859").
-define(secret, "0ecadd6f764fbfa43073ea89105be939").
-define(auth_url, "https://www.facebook.com/dialog/oauth?client_id=").

authorize_url() ->
	T = [{"oauth_token_secret", _TokenSecret}, {"oauth_token", Token}] = request_token(),
	URL = ?auth_url ++ Token,
	{ok, #skr_auth_reqtoken{provider=?provider, url=URL, token=T}}.

access_token(#skr_auth_reqtoken{provider=?provider, token=[{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}]}) ->
	T = access_token(?key, ?secret, Token, TokenSecret),
	{ok, #skr_auth_acctoken{provider=?provider, token=T}}.

account_info(#skr_auth_acctoken{provider=?provider, token=[{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}, {"uid", _Uid}]}=AT) ->
	Response = account_info(?key, ?secret, Token, TokenSecret),
	Resp = jiffy:decode(Response),
	{ok, to_account(Resp, AT)}.

%% ====================================================================
%% Internal functions
%% ====================================================================
request_token() ->
	request_token(?key, ?secret).

to_account(A, _B) ->
	ok = A.
%% ====================================================================
%% Google functions
%% ====================================================================

oauth_get(URL, Params, Consumer, Token, TokenSecret) ->
  Signed = oauth:sign("GET", URL, Params, Consumer, Token, TokenSecret),
  {AuthorizationParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Request = {oauth:uri(URL, QueryParams), [oauth:header(AuthorizationParams)]},
  httpc:request(get, Request, [{autoredirect, false}], []).

request_token(Key, Secret) -> 
  oauth_get("https://www.google.com/accounts/OAuthGetRequestToken", [{"scope", "http://www.google.com/calendar/feeds/"}], {Key, Secret, hmac_sha1}, "", "").

access_token(Key, Secret, Token, TokenSecret) ->
  {ok, AccessToken} = oauth:post("https://www.google.com/accounts/OAuthGetAccessToken", [], {Key, Secret, hmac_sha1}, Token, TokenSecret),
  oauth:params_decode(AccessToken).

account_info(Key, Secret, Token, TokenSecret) ->
  {ok, {_, _, AccountInfo}} = oauth:get("https://www.googleapis.com/drive/v2/about", [], {Key, Secret, hmac_sha1}, Token, TokenSecret),
  AccountInfo. 