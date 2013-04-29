-module(skyraid_webmachine_login_resource).

-include_lib("skyraid/include/skyraid.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/1, allowed_methods/2, content_types_provided/2, allow_missing_post/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, []}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}], ReqData, Context}.

allow_missing_post(ReqData, Context) ->
	{true, ReqData, Context}.

process_post(ReqData, Context) ->
	Body = wrq:req_body(ReqData),
	{struct, [{<<"username">>, Username}, {<<"password">>, Password}]} = mochijson2:decode(Body),
	Response = case skyraid:login(Username, Password) of
						{ok, SessionRef} ->
							{ok, _SessionInfo} = skyraid_user_session:info(SessionRef),
							[{status, ok}, {session, pid_to_list(SessionRef)}, {user, "Adam"}];
						{error, Error} -> [{status, error}, {error, Error}]
				end,
	Json = mochijson2:encode(Response),
	{true, wrq:append_to_response_body(Json, ReqData), Context}.