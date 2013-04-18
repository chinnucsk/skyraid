-module(skyraid_webmachine_user_resource).

-include_lib("skyraid/include/skyraid.hrl").
-include_lib("jsonerl/src/jsonerl.hrl").

-define(list_to_json(RecordName, List), "[" ++ string:join( [ [?record_to_json(RecordName, Rec)] || Rec <- List], ",\n") ++ "]").

-export([init/1, content_types_provided/2, resource_exists/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, []}.

content_types_provided(ReqData, Context) ->
	erlang:display("user resource"),
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
	{ ?record_to_json(skr_user, User), ReqData, User};

to_json(ReqData, Users) when is_list(Users)->
	{ ?list_to_json(skr_user, Users), ReqData, Users}.

