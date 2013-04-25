-module(skyraid_webmachine_user_resource).

-include_lib("skyraid/include/skyraid.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1, content_types_provided/2, resource_exists/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, []}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}], ReqData, Context}.

resource_exists(ReqData, Context) ->
	case wrq:disp_path(ReqData) of
		"" ->
			{ok, All} = skyraid_user_repo:get_all(),
			{true, ReqData, All};
		UserId ->
			case tracktrain_user_repo:get(list_to_integer(UserId)) of
				{ok, User} -> {true, ReqData, User};
				not_found -> {false, ReqData, Context}
			end
	end.

to_json(_ReqData, []) ->
	{"", _ReqData, []};

to_json(ReqData, [User]) ->
	{ mochijson2:encode(user_to_proplist(User)), ReqData, User};

to_json(ReqData, Users) when is_list(Users)->
	UP = [user_to_proplist(U) || U <- Users],
	Json = mochijson2:encode(UP),
	{ Json, ReqData, Users}.

user_to_proplist(#skr_user{id=Id, username=Username, display_name=DisplayName}) ->
	[{id, Id}, {username, Username}, {display_name, DisplayName}].

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
 	{struct, [{<<"id">>, _}, {<<"username">>, <<"Apa">>}, {<<"display_name">>, <<"ApaDisplay">>}]} = mochijson2:decode(Json).

 -endif.