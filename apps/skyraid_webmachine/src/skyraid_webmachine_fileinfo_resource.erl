-module(skyraid_webmachine_fileinfo_resource).

-include("../../skyraid/include/skyraid.hrl").

-export([init/1, allowed_methods/2, content_types_provided/2, is_authorized/2, resource_exists/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% ====================================================================
%% API
%% ====================================================================

init([]) -> 
	{ok, []}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}], ReqData, Context}.

is_authorized(ReqData, Context) ->
	?DEBUG("is_authorized"),
	case wrq:get_req_header("Authorization", ReqData) of
		undefined -> 
			{true, ReqData, Context};
		Token -> 
			Session = binary_to_term(base64:decode(Token)),
			?DEBUG(Session),
			case skyraid_auth:info(Session) of
				{ok, _Info} ->
					{true, ReqData, Context};
				{error, session_not_found} ->
					{false, ReqData, Context}
			end
	end.

resource_exists(ReqData, _Context) ->
	Session = binary_to_term(base64:decode(wrq:get_req_header("Authorization", ReqData))),
	Path = wrq:disp_path(ReqData),
	AccountId = wrq:path_info(account_id, ReqData),
	?DEBUG({Session, Path, AccountId}),
	{ok, Files} = skyraid:file_list(Session, AccountId), 	
	{true, ReqData, Files}.

to_json(ReqData, Context) ->
	{<<"here comes the file">>, ReqData, Context}.

%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

skyraid_webmachine_fileinfo_resource_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			{"Get a list of all files from a specified account", fun list_account_root_tc/0}
		]
	}.

setup() ->
	application:set_env(skyraid_webmachine, ip, "127.0.0.1"),
	application:set_env(skyraid_webmachine, port, 8000),
	ok = skyraid_webmachine:start().

teardown(_Any) ->
	ok = skyraid_webmachine:stop().

list_account_root_tc() ->
	Login = "{\"username\":\"Adam\", \"password\": \"test\"}",
	{200,[_,{<<"sessionId">>, SessionId}, _, _]} = skyraid_webmachine_rest:rest_req(post, "http://localhost:8000/api/login", Login),
	Header = [{"Authorization", binary_to_list(SessionId)}],
	{ok, _Resp} = httpc:request(get, {"http://localhost:8000/api/account/0/file_info/here/is/my/path", Header}, [],[]).

 -endif.
