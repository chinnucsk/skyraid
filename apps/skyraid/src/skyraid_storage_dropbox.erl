-module(skyraid_storage_dropbox).

-include("skyraid.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(key, "99s6c0nh6oeez3m").
-define(secret, "obv1gjb2aoq1zg6").
-define(auth_url, "https://www.dropbox.com/1/oauth/authorize?oauth_token=").

-export([authorize_url/0, access_token/1, account_info/1]).

authorize_url() ->
	T = [{"oauth_token_secret", _TokenSecret}, {"oauth_token", Token}] = request_token(),
	{ok, {{url, ?auth_url ++ Token}, {request_token, T}}}.

access_token([{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}]) ->
	AccessToken = dropbox:access_token(?key, ?secret, Token, TokenSecret),
	{ok, {access_token, AccessToken}}.

account_info([{"oauth_token_secret", TokenSecret}, {"oauth_token", Token}, {"uid", _Uid}]) ->
	Response = dropbox:account_info(?key, ?secret, Token, TokenSecret),
	Resp = jiffy:decode(Response),
	{ok, to_account(Resp)}.

%% ====================================================================
%% Internal functions
%% ====================================================================

request_token() ->
	dropbox:request_token(?key, ?secret).

to_account({Account}) ->
	build(Account, #skr_account{}).

build([], A) ->
	A;
build([{<<"display_name">>, Value} | Rest], A) ->
	build(Rest, A#skr_account{display_name=Value});
build([{<<"uid">>, Value} | Rest], A) ->
	build(Rest, A#skr_account{ext_id=Value});
build([{<<"country">>, Value} | Rest], A) ->
	build(Rest, A#skr_account{country=Value});
build([{<<"quota_info">>, {[{<<"shared">>,Shared},{<<"quota">>,Quota},{<<"normal">>,Normal}]}} | Rest], A) ->
	build(Rest, A#skr_account{quota_info=#skr_quota_info{shared=Shared, quota=Quota, normal=Normal}});
build([_Any | R], A) ->
	build(R, A).


%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).

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
	
 	#skr_account{display_name = <<"Apa Nilsson">>} = to_account(A).

 -endif.