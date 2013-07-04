-module(skyraid_webmachine_fileinfo_resource).

-include("../../skyraid/include/skyraid.hrl").

-export([init/1, allowed_methods/2, content_types_provided/2, is_authorized/2, resource_exists/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(WWW_AUTH, "Basic realm=Skyraid").
%% ====================================================================
%% State
%% ====================================================================
-record(state, {
	session,
	files = []
}).

%% ====================================================================
%% API
%% ====================================================================

init([]) -> 
	{ok, #state{}}.

allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
	{[{"application/json", to_json}], ReqData, State}.

is_authorized(ReqData, State) ->
	case wrq:get_req_header("Authorization", ReqData) of
		undefined -> 
			{?WWW_AUTH, ReqData, State};
		Token -> 
			case decode_session(Token) of
				{ok, Session} ->
					case skyraid_auth:info(Session) of
						{ok, _Info} ->
							{true, ReqData, State#state{session=Session}};
						{error, session_not_found} ->
							{?WWW_AUTH, ReqData, State}
					end;
				{error, _Reason} ->
					{?WWW_AUTH, ReqData, State}
			end
	end.

resource_exists(ReqData, #state{session=Session}=S) ->
	Path = wrq:disp_path(ReqData),
	AccountId = wrq:path_info(account_id, ReqData),
	Result = case skyraid:file_list(Session, AccountId, Path) of
				{ok, Files} ->
					{true, ReqData, S#state{files=Files}};
				{error, _Reason} ->
					{false, ReqData, S}
			end,
	Result.

to_json(ReqData, #state{files=Files}=S) ->
	FP = fileinfos_to_proplist(Files),
	Json = mochijson2:encode(FP),
	{Json, ReqData, S}.

%% ====================================================================
%% Private functions 
%% ====================================================================
decode_session(undefined) ->
	{error, undefined};

decode_session(SessionStr) ->
	try binary_to_term(base64:decode(SessionStr)) of
		Session -> {ok, Session}
	catch
		error:Error -> {error, Error}
	end.

fileinfos_to_proplist(Files) when is_list(Files) ->
	[fileinfo_to_proplist(F) || F <- Files].

fileinfo_to_proplist(#skr_file_info{path=Path, is_dir=IsDir, size=Size}) ->
	[{path, Path}, {is_dir, IsDir}, {size, Size}].

%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

skyraid_webmachine_fileinfo_resource_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			{"Get file from invalid session", fun list_account_invalid_session_tc/0},
			{"Get file without session", fun list_account_without_session_tc/0},
			{"Get a list of all files from a specified account", fun list_account_root_tc/0}
		]
	}.

setup() ->
	application:set_env(skyraid_webmachine, ip, "127.0.0.1"),
	application:set_env(skyraid_webmachine, port, 8000),
	ok = skyraid_webmachine:start().

teardown(_Any) ->
	ok = skyraid_webmachine:stop().

list_account_without_session_tc() ->
	Url = "http://localhost:8000/api/account/0/file_info/",
	Header = [],
	{401, _} = skyraid_webmachine_rest:get(Url, Header).

list_account_invalid_session_tc() ->
	Url = "http://localhost:8000/api/account/0/file_info/",
	Header = [{"Authorization", "Invalid"}],
	{401, _} = skyraid_webmachine_rest:get(Url, Header).

list_account_root_tc() ->
	Login = "{\"username\":\"Adam\", \"password\": \"test\"}",
	{200,[_,{<<"sessionId">>, SessionId}, _, _]} = skyraid_webmachine_rest:rest_req(post, "http://localhost:8000/api/login", Login),
	Url = "http://localhost:8000/api/account/0/file_info/",
	Header = [{"Authorization", binary_to_list(SessionId)}],
	{200, [{struct, [{<<"path">>, _},{<<"is_dir">>,_},{<<"size">>,_}]} |_Rest]} = skyraid_webmachine_rest:get(Url, Header).

list_account_specified_dir_tc() ->
	Login = "{\"username\":\"Adam\", \"password\": \"test\"}",
	{200,[_,{<<"sessionId">>, SessionId}, _, _]} = skyraid_webmachine_rest:rest_req(post, "http://localhost:8000/api/login", Login),
	Url = "http://localhost:8000/api/account/0/file_info/sub_dir/",
	Header = [{"Authorization", binary_to_list(SessionId)}],
	{200, [{struct, [{<<"path">>, _},{<<"is_dir">>,_},{<<"size">>,_}]} |_Rest]} = skyraid_webmachine_rest:get(Url, Header).

decode_session_test() ->
	Str = "g2dkAA1ub25vZGVAbm9ob3N0AAAABAAAAAEA",
	{ok, _Session} = decode_session(Str),
	{error, undefined} = decode_session(undefined),
	{error, _Reason} = decode_session("Invalid").

 -endif.
