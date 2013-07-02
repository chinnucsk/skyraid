-module(skyraid_webmachine_file_resource).

-include("../../skyraid/include/skyraid.hrl").

-export([init/1, allowed_methods/2, content_types_provided/2, content_types_accepted/2, is_authorized/2, resource_exists/2, to_text/2, from_text/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% ====================================================================
%% API
%% ====================================================================

init([]) -> {ok, []}.

allowed_methods(ReqData, Context) ->
    {['GET', 'PUT'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
	{[{"text/plain", to_text}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
	{[{"text/plain", from_text}], ReqData, Context}.

is_authorized(ReqData, Context) ->
	case wrq:get_req_header("Authorization", ReqData)of
		undefined -> 
			{true, ReqData, Context};
		Token -> 
			_Session = binary_to_term(base64:decode(Token)),
			{true, ReqData, Context}
	end.

resource_exists(ReqData, Context) ->
	?DEBUG(decode(ReqData)),
	{true, ReqData, Context}.

to_text(ReqData, Context) ->
	{<<"here comes the file">>, ReqData, Context}.

from_text(ReqData, Context) ->
	%% Just echo the content for now
	Bin = wrq:req_body(ReqData),
	{true, wrq:append_to_response_body(Bin, ReqData), Context}.

decode(ReqData) ->
	[_, Path] = re:split(wrq:path(ReqData), "file"),
	{
		wrq:path_info(user_id, ReqData), 
		wrq:path_info(account_id, ReqData),
		binary_to_list(Path)
	}.

%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

skyraid_webmachine_file_resource_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			{"Get a user file from a specified account", fun get_user_file_from_account_tc/0}
		]
	}.

setup() ->
	application:set_env(skyraid_webmachine, ip, "127.0.0.1"),
	application:set_env(skyraid_webmachine, port, 8000),
	ok = skyraid_webmachine:start().

teardown(_Any) ->
	ok = skyraid_webmachine:stop().

get_user_file_from_account_tc() ->
	ok.
	%%{200, "here comes the file"} = skyraid_webmachine_rest:rest_req(text, "http://localhost:8000/api/account/0/file/myFolder/hello.txt").

 -endif.