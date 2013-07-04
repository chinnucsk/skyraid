-module(skyraid_webmachine_account_resource).

-include("../../skyraid/include/skyraid.hrl").

-ifdef(TEST).
-compile({no_auto_import,[get/1]}).
-import(skyraid_webmachine_rest, [get/1, get/2]).
-endif.

-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2, resource_exists/2, to_json/2, from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% ====================================================================
%% API
%% ====================================================================

init([]) -> {ok, []}.

allowed_methods(ReqData, Context) ->
    {['GET', 'HEAD', 'PUT'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

resource_exists(ReqData, Context) ->
	case string:tokens(wrq:disp_path(ReqData), "/") of
		[] ->
			{ok, All} = skyraid_account_repo:get_all_accounts(),
			{true, ReqData, All};
		[_UserId, AccountId] ->
			case skyraid_account_repo:get_account(AccountId) of
				{ok, Account} -> {true, ReqData, [Account]};
				not_found -> {false, ReqData, Context}
			end;
		[UserId] ->
			case skyraid_account_repo:get_accounts(UserId) of
				{ok, Accounts} -> 
					{true, ReqData, Accounts};
				not_found -> {false, ReqData, Context}
			end;
		_Any -> 
			{false, ReqData, Context}
	end.

to_json(ReqData, Accounts) when is_list(Accounts) ->
	UP = [account_to_proplist(U) || U <- Accounts],
	Json = mochijson2:encode([{status, ok}, {accounts, UP}]),
	{Json, ReqData, Accounts}.

from_json(ReqData, Context) ->
	{struct, AccountPropList} = mochijson2:decode(wrq:req_body(ReqData)),
	Account = proplist_to_account(AccountPropList),
	Response = case skyraid_account_repo:new(Account) of
		{ok, #skr_account{id=ID}} -> [{status, ok}, {userId, ID}];
		{error, Error} -> [{status, error}, {error, Error}]
	end,
	Json = mochijson2:encode(Response),
	{true, wrq:append_to_response_body(Json, ReqData), Context}.

%% ====================================================================
%% Private
%% ====================================================================

account_to_proplist(#skr_account{id=Id, user_id=UserId, display_name=DisplayName}) ->
	[{id, Id}, {userId, UserId}, {displayName, DisplayName}].

proplist_to_account([{<<"userId">>, UserId}, {<<"displayName">>, DisplayName}, {<<"email">>, Email}]) ->
	#skr_account{user_id=UserId, display_name=DisplayName, email=Email}.


%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

skyraid_webmachine_account_resource_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			{"Get all accounts", fun get_all_accounts_tc/0},
			{"Get all accounts for specified user", fun get_all_user_accounts_tc/0},
			{"Get specified user account", fun get_specific_user_account_tc/0}
		]
	}.

setup() ->
	application:set_env(skyraid_webmachine, ip, "127.0.0.1"),
	application:set_env(skyraid_webmachine, port, 8000),
	ok = skyraid_webmachine:start().

teardown(_Any) ->
	ok = skyraid_webmachine:stop().

get_all_accounts_tc() ->
	{200, {struct, [{<<"status">>,<<"ok">>}, {<<"accounts">>, _ }]}} = get("http://localhost:8000/api/accounts").

get_all_user_accounts_tc() ->
	{200, {struct, [{<<"status">>,<<"ok">>}, {<<"accounts">>, _ }]}} = get("http://localhost:8000/api/account/0").

get_specific_user_account_tc() ->
	{200, {struct, [{<<"status">>,<<"ok">>}, {<<"accounts">>, _ }]}} = get("http://localhost:8000/api/user/0/account/0").	
-endif.