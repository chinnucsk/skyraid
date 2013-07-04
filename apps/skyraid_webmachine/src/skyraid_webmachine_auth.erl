-module(skyraid_webmachine_auth).

-include("../../skyraid/include/skyraid.hrl").

-export([validate/1]).

%% ====================================================================
%% API functions 
%% ====================================================================
-spec validate(string()) -> {ok, pid()} | {error, term()}.
validate(SessionStr) ->
	case decode(SessionStr) of
		{ok, SessionRef} -> 
			case skyraid_auth:info(SessionRef) of
				{ok, #skr_session_info{user_id=UserId}} -> {ok, UserId, SessionRef};
				{error, Error} -> {error, Error}
			end;
		{error, Error} -> {error, Error}
	end.

%% ====================================================================
%% Private functions 
%% ====================================================================
decode(undefined) ->
	{error, undefined};

decode(SessionStr) ->
	try binary_to_term(base64:decode(SessionStr)) of
		Session -> {ok, Session}
	catch
		error:Error -> {error, Error}
	end.

%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

skyraid_webmachine_account_resource_test_() ->
	{setup, fun setup/0, fun teardown/1,
		[
			{"Validate logged in user", fun validate_loggedin_user_tc/0},
			{"Validate not logged in user", fun validate_not_loggedin_user_tc/0}
		]
	}.

setup() ->
	application:set_env(skyraid_webmachine, ip, "127.0.0.1"),
	application:set_env(skyraid_webmachine, port, 8000),
	ok = skyraid_webmachine:start().

teardown(_Any) ->
	ok = skyraid_webmachine:stop().

validate_loggedin_user_tc() ->
	{ok, SessionRef} = skyraid:login(<<"Adam">>, <<"test">>),
	SessionStr = base64:encode(term_to_binary(SessionRef)),
	?DEBUG(SessionStr),
	{ok, "0", SessionRef} = validate(SessionStr).

validate_not_loggedin_user_tc() ->
	SessionStr = "g2dkAA1ub25vZGVAbm9ob3N0AAAABAAAAAEA",
	{error, session_not_found} = validate(SessionStr).

decode_test() ->
	Str = "g2dkAA1ub25vZGVAbm9ob3N0AAAABAAAAAEA",
	Pid = list_to_pid("<0.4.1>"),
	?assertEqual({ok, Pid}, decode(Str)),
	?assertEqual({error, undefined}, decode(undefined)),
	?assertEqual({error, function_clause}, decode("Invalid")).

-endif.