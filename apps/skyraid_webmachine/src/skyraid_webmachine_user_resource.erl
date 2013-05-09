-module(skyraid_webmachine_user_resource).

-include_lib("skyraid/include/skyraid.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2, resource_exists/2, to_json/2, from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% ====================================================================
%% API
%% ====================================================================

init([]) -> {ok, []}.

allowed_methods(ReqData, Context) ->
    {['GET', 'PUT'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}], ReqData, Context}.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}], RD, Ctx}.

resource_exists(ReqData, Context) ->
	case wrq:disp_path(ReqData) of
		"" ->
			{ok, All} = skyraid_user_repo:get_all(),
			{true, ReqData, All};
		UserId ->
			case skyraid_user_repo:get(list_to_integer(UserId)) of
				{ok, User} -> {true, ReqData, User};
				not_found -> {false, ReqData, Context}
			end
	end.

to_json(_ReqData, []) ->
	{"", _ReqData, []};

to_json(ReqData, [User]) ->
	{ mochijson2:encode(user_to_proplist(User)), ReqData, User};

to_json(ReqData, Users) when is_list(Users) ->
	UP = [user_to_proplist(U) || U <- Users],
	Json = mochijson2:encode(UP),
	{ Json, ReqData, Users}.

from_json(ReqData, Context) ->
	?DEBUG("Register user"), 
	{struct, UserPropList} = mochijson2:decode(wrq:req_body(ReqData)),
	User = proplist_to_user(UserPropList),
	Response = case skyraid_user_repo:new(User) of
		{ok, #skr_user{id=ID}} -> [{status, ok}, {userId, ID}];
		{error, Error} -> [{status, error}, {error, Error}]
	end,
	Json = mochijson2:encode(Response),
	{true, wrq:append_to_response_body(Json, ReqData), Context}.

%% ====================================================================
%% Private
%% ====================================================================

user_to_proplist(#skr_user{id=Id, username=Username, display_name=DisplayName}) ->
	[{id, Id}, {username, Username}, {displayName, DisplayName}].

proplist_to_user([{<<"username">>, Username}, {<<"password">>, Password}, {<<"email">>, Email}]) ->
	#skr_user{username=Username, password=Password, email=Email}.


%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).

to_json_test() ->
	User = #skr_user{username = <<"Apa">>,
			  password = <<"test">>, 
			  display_name = <<"ApaDisplay">>, 
			  email = <<"adam@gmail.com">>},
	
 	{Json, _, _} = to_json([], [User]),
 	{struct, [{<<"id">>, _}, {<<"username">>, <<"Apa">>}, {<<"displayName">>, <<"ApaDisplay">>}]} = mochijson2:decode(Json).

 -endif.