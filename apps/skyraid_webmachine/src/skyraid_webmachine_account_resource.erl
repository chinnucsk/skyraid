-module(skyraid_webmachine_account_resource).

-include_lib("skyraid/include/skyraid.hrl").

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
		[UserId, AccountId] ->
			case skyraid_account_repo:get_account(UserId, AccountId) of
				{ok, Account} -> {true, ReqData, [Account]};
				not_found -> {false, ReqData, Context}
			end;
		[UserId] ->
			case skyraid_account_repo:get_accounts(UserId) of
				{ok, Accounts} -> 
					?DEBUG(Accounts),
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

account_to_proplist(#skr_account{id={UserId, Id}, user_id=UserId, display_name=DisplayName}) ->
	[{id, Id}, {userId, UserId}, {displayName, DisplayName}].

proplist_to_account([{<<"userId">>, UserId}, {<<"displayName">>, DisplayName}, {<<"email">>, Email}]) ->
	#skr_account{user_id=UserId, display_name=DisplayName, email=Email}.


%% ====================================================================
%% Unit Tests 
%% ====================================================================
