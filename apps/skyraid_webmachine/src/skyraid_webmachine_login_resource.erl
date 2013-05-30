-module(skyraid_webmachine_login_resource).

-include_lib("skyraid/include/skyraid.hrl").

-export([init/1, allowed_methods/2, content_types_provided/2, allow_missing_post/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% ====================================================================
%% Webmachine Callbacks 
%% ====================================================================
init([]) -> {ok, []}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
	{[{"application/json", to_json}], ReqData, Context}.

allow_missing_post(ReqData, Context) ->
	{true, ReqData, Context}.

process_post(ReqData, Context) ->
	Body = wrq:req_body(ReqData),
	%% TODO validate this and return 400
	{struct, [{<<"username">>, Username}, {<<"password">>, Password}]} = mochijson2:decode(Body),
	{Result, JSON} = case skyraid:login(Username, Password) of
							{ok, SessionRef} ->
								{ok, SessionInfo} = skyraid_user_session:info(SessionRef),
								{true, to_json(SessionRef, SessionInfo)};
								%%{true, [{status, ok}, {sessionId, pid_to_list(SessionRef)}, session_info_to_proplist(S)]};
							{error, Error} -> 
								{{halt, 500}, to_json({error, Error})}
						end,
	{Result, wrq:append_to_response_body(JSON, ReqData), Context}.

%% ====================================================================
%% Private Functions 
%% ====================================================================
to_json(SessionRef, SessionInfo) ->
	{User, Accounts} = session_info_to_proplist(SessionInfo),
	PL = [{status, ok}, {sessionId, base64:encode(term_to_binary(SessionRef))}, User, Accounts],
	mochijson2:encode(PL).

to_json({error, Error}) ->
	mochijson2:encode([{status, error}, {error, Error}]).

session_info_to_proplist(#skr_session_info{user=User, accounts=Accounts}) ->
	#skr_user{display_name=DisplayName, email=Email} = User,
	UP = {user, [{<<"displayName">>, DisplayName}, {<<"email">>, Email}]},
	AP = {accounts, accounts_to_proplist(Accounts)},
	{UP, AP}.
	
accounts_to_proplist([Accounts]) ->
	[AP] = [account_to_proplist(X) || X <- [Accounts]],
	AP;
accounts_to_proplist(_Any) ->
	[].

account_to_proplist(#skr_account{id={UserId, Id}, display_name=DisplayName, storage_id=StorageId}) ->
	[{id, list_to_binary(UserId ++ "." ++ Id)}, {name, list_to_binary(DisplayName)}, {provider, StorageId}].


%% ====================================================================
%% Unit Tests 
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_json_normal_test() ->
	Adam = #skr_user{
			id="0",
			username = <<"Adam">>, 
			password = <<"test">>, 
			display_name = <<"AdamDisplay">>, 
			email = <<"adam@gmail.com">>},

	AdamAccount1 = #skr_account {
						id={"0", "0"},
						user_id="0",
						display_name="AdamAccount1",
						storage_id=ftp,
						authentication = #skr_auth_basic {
							url="ftp://myftp",
							provider=ftp,
							username="User",
							password="test"
						}
					},

	SessionRef = list_to_pid("<0.4.1>"),					
	SessionInfo = #skr_session_info{timestamp=12, user=Adam, accounts=[AdamAccount1]},

	Expected = {struct,[{<<"status">>,<<"ok">>},
                   		{<<"sessionId">>,<<"g2dkAA1ub25vZGVAbm9ob3N0AAAABAAAAAEA">>},
                   		{<<"user">>,{struct,[{<<"displayName">>,<<"AdamDisplay">>},{<<"email">>,<<"adam@gmail.com">>}]}},
                   		{<<"accounts">>,{struct,[{<<"id">>,<<"0.0">>},{<<"name">>,<<"AdamAccount1">>}, {<<"provider">>, <<"ftp">>}]}}]},

    JSON = to_json(SessionRef, SessionInfo),
    Expected = mochijson2:decode(JSON).

to_json_no_account_test() ->
	Adam = #skr_user{
			id="0",
			username = <<"Adam">>, 
			password = <<"test">>, 
			display_name = <<"AdamDisplay">>, 
			email = <<"adam@gmail.com">>},

	SessionRef = list_to_pid("<0.4.1>"),					
	SessionInfo = #skr_session_info{timestamp=12, user=Adam, accounts=[]},

	Expected = {struct,[{<<"status">>,<<"ok">>},
                   		{<<"sessionId">>,<<"g2dkAA1ub25vZGVAbm9ob3N0AAAABAAAAAEA">>},
                   		{<<"user">>,{struct,[{<<"displayName">>,<<"AdamDisplay">>},{<<"email">>,<<"adam@gmail.com">>}]}},
                   		{<<"accounts">>,[]}]},

    JSON = to_json(SessionRef, SessionInfo),
    Expected = mochijson2:decode(JSON).

to_json_error_test() ->
	Expected = {struct,[{<<"status">>,<<"error">>},{<<"error">>,"ErrorMsg"}]},
	JSON = to_json({error, "ErrorMsg"}),
	Expected = mochijson2:decode(JSON).

-endif.