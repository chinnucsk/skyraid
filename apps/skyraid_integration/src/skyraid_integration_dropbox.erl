-module(skyraid_integration_dropbox).

-include_lib("../../skyraid/include/skyraid.hrl").

-define(key, "6b8S6hN6SK2YvYySTfpVg").
-define(secret, "obv1gjb2aoq1zg6").
-define(auth_url, "https://www.dropbox.com/1/oauth/authorize?oauth_token=").

-behaviour(skyraid_account_provider).
-export([init/0, create_token/0, authenticate/1, logout/1, account_info/1]).

-behaviour(skyraid_file_provider).
-export([list_files/1, write_file/3, read_file/2]).

%% ====================================================================
%% API account provider functions
%% ====================================================================
init() ->
	{ok, []}.

create_token() ->
	T = [{"oauth_token_secret", _TokenSecret}, {"oauth_token", Token}] = request_token(),
	URL = ?auth_url ++ Token,
	{ok, #skr_auth_reqtoken{provider=dropbox, url=URL, token=T}}.

authenticate(#skr_auth_reqtoken{provider=dropbox, token=[{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}]}) ->
	T = access_token(?key, ?secret, Token, TokenSecret),
	{ok, #skr_auth_acctoken{provider=dropbox, token=T}}.

logout(_Session) ->
	ok.

account_info(#skr_auth_acctoken{provider=dropbox, token=[{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}, {"uid", _Uid}]}=AT) ->
	Response = account_info(?key, ?secret, Token, TokenSecret),
	Resp = mochijson2:decode(Response),
	{ok, to_account(Resp, AT)}.

%% ====================================================================
%% API files provider functions
%% ====================================================================

list_files(#skr_auth_acctoken{provider=dropbox, token=[{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}, {"uid", _Uid}]}) ->
	Response = metadata(?key, ?secret, Token, TokenSecret, "dropbox", ""),
	Resp = mochijson2:decode(Response),
	{ok, to_files(Resp)}.

write_file(#skr_auth_acctoken{provider=dropbox, token=[{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}, {"uid", _Uid}]}, FileName, Content) ->
	file_put(?key, ?secret, Token, TokenSecret, "dropbox", FileName, Content).

read_file(#skr_auth_acctoken{provider=dropbox, token=[{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}, {"uid", _Uid}]}, FileName) ->
	file_get(?key, ?secret, Token, TokenSecret, "dropbox", FileName).

%% ====================================================================
%% Internal functions
%% ====================================================================

request_token() ->
	request_token(?key, ?secret).

to_account({Account}, AccessToken) ->
	build_account(Account, #skr_account{provider=dropbox, authentication=AccessToken}).

build_account([], A) ->
	A;
build_account([{<<"display_name">>, Value} | Rest], A) ->
	build_account(Rest, A#skr_account{display_name=Value});
build_account([{<<"uid">>, Value} | Rest], A) ->
	build_account(Rest, A#skr_account{id={dropbox, Value}, ext_id=Value});
build_account([{<<"country">>, Value} | Rest], A) ->
	build_account(Rest, A#skr_account{country=Value});
build_account([{<<"quota_info">>, {[{<<"shared">>,Shared},{<<"quota">>,Quota},{<<"normal">>,Normal}]}} | Rest], A) ->
	build_account(Rest, A#skr_account{quota_info=#skr_quota_info{shared=Shared, quota=Quota, normal=Normal}});
build_account([_Any | R], A) ->
	build_account(R, A).

to_files({Files}) ->
	%% TODO implement converter
	{ok, Files}.

%% --------------------------------------------------------------------
%% Dropbox functions
%% --------------------------------------------------------------------
request_token(Key, Secret) -> 
  {ok, RequestToken} = oauth:post("https://api.dropbox.com/1/oauth/request_token", [], {Key, Secret, hmac_sha1}),
  oauth:params_decode(RequestToken).

%%authorize(Key, Secret, Token, TokenSecret, Callback) ->
%%  {ok, Authorize} = oauth:get("https://www.dropbox.com/1/oauth/authorize", [{"oauth_callback", Callback}], {Key, Secret, hmac_sha1}, Token, TokenSecret),
%%  oauth:params_decode(Authorize).

access_token(Key, Secret, Token, TokenSecret) ->
  {ok, AccessToken} = oauth:post("https://api.dropbox.com/1/oauth/access_token", [], {Key, Secret, hmac_sha1}, Token, TokenSecret),
  oauth:params_decode(AccessToken).

account_info(Key, Secret, Token, TokenSecret) ->
  {ok, {_, _, AccountInfo}} = oauth:get("https://api.dropbox.com/1/account/info", [], {Key, Secret, hmac_sha1}, Token, TokenSecret),
  AccountInfo.

file_get(Key, Secret, Token, TokenSecret, Root, Path) ->
  {ok, {_, _, File}} = oauth:get("https://api-content.dropbox.com/1/files/" ++ Root ++ "/" ++ Path, [], {Key, Secret, hmac_sha1}, Token, TokenSecret),
  {ok, File}.

file_put(Key, Secret, Token, TokenSecret, Root, Path, Content) ->
  {ok, {_, _, File}} = oauth:put("https://api-content.dropbox.com/1/files_put/" ++ Root ++ "/" ++ Path, [], {"application/binary", Content}, {Key, Secret, hmac_sha1}, Token, TokenSecret),
  {ok, File}.

metadata(Key, Secret, Token, TokenSecret, Root, Path) ->
  {ok, {_, _, Metadata}} = oauth:get("https://api.dropbox.com/1/metadata/" ++ Root ++ "/" ++ Path, [], {Key, Secret, hmac_sha1}, Token, TokenSecret),
  Metadata. 

%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

to_account_test() ->
	A = {[{<<"referral_link">>,<<"https://www.dropbox.com/referrals/NTE1NDg0Mzc1MDk">>},
	  		{<<"display_name">>,<<"Apa Nilsson">>},
	  		{<<"uid">>,154843750},
	  		{<<"country">>,<<"SE">>},
	  		{<<"quota_info">>,
	  		{[{<<"shared">>,0},
	    		{<<"quota">>,2147483648},
	    		{<<"normal">>,1425347}]}},
	    		{<<"email">>,<<"apa.nilsson@gmail.com">>}]},
	
 	#skr_account{display_name = <<"Apa Nilsson">>} = to_account(A, #skr_account{provider=dropbox}).

 -endif.