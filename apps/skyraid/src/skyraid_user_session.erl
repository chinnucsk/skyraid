%% Copyright
%%
%% @doc Handles all request for a single authenticated user
%%
-module(skyraid_user_session).
-include("skyraid.hrl").
-behaviour(gen_server).
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, stop/1, info/1, add_account/2, add_accounts/2, get_account/2, get_authentication/2]).

start_link(User) ->
	gen_server:start_link(?MODULE, [User], []).

stop(SessionRef) when is_pid(SessionRef) ->
    gen_server:call(SessionRef, stop).

info(SessionRef) when is_pid(SessionRef) ->
	gen_server:call(SessionRef, get_info).

add_account(SessionRef, #skr_account{}=A) ->
    gen_server:call(SessionRef, {add_account, A}).

add_accounts(SessionRef, [Accounts]) ->
    gen_server:call(SessionRef, {add_accounts, [Accounts]}).

get_account(SessionRef, AccountID) ->
    gen_server:call(SessionRef, {get_account, AccountID}).

get_authentication(SessionRef, AccountID) ->
    gen_server:call(SessionRef, {get_authentication, AccountID}).

%% ====================================================================
%% State
%% ====================================================================
-record(state,
{
    timestamp = erlang:now(),
    user,
    accounts
}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([#skr_user{id=UserId}=User]) ->
    {ok, Accounts} = skyraid_account_repo:get_accounts(UserId),
    {ok, #state{user=User, accounts=Accounts}}.

handle_call(get_info, _From, S) ->
    Info = create_session_info(S),
    {reply, {ok, Info}, S};

handle_call({add_account, NewAccount = #skr_account{}}, _From, #state{accounts=CurrentAccounts}=S) ->
    %% TODO add check for already existing or?
    NewState = S#state{accounts= [NewAccount] ++ CurrentAccounts},
    Info = create_session_info(NewState),
    {reply, {ok, Info}, NewState};

handle_call({add_accounts, [NewAccounts]}, _From, #state{accounts=CurrentAccounts}=S) ->
    %% TODO add check for already existing or?
    NewState = S#state{accounts=[NewAccounts] ++ CurrentAccounts},
    Info = create_session_info(NewState),
    {reply, {ok, Info}, NewState};

handle_call({get_account, AccountID}, _From, #state{accounts=Accounts}=S) ->
    Replay = case [A || #skr_account{id=ID}=A <- Accounts, AccountID == ID] of
              [A|_Rest] -> {ok, A};
              [] -> {error, account_not_found}
           end,
    {reply, Replay, S};

handle_call({get_authentication, AccountID}, _From, #state{accounts=Accounts}=S) ->
    Replay = case [Auth || #skr_account{id=ID, authentication=Auth} <- Accounts, AccountID == ID] of
		      [Auth] -> {ok, Auth};
		      [] -> {error, account_not_found}
	       end,
    {reply, Replay, S};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

create_session_info(#state{timestamp=TimeStamp,
			   user=User,
			   accounts=Accounts}) ->
    #skr_session_info{timestamp=TimeStamp,
		      user=User,
		      accounts=Accounts}.

%% ====================================================================
%% Unit tests
%% ====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

skyraid_user_session_test_() ->
    {setup, fun setup/0, fun teardown/1, 
        fun(SessionRef) ->
            [ 
                get_authentication_tc(SessionRef),
                get_account_tc(SessionRef),
                add_account_tc(SessionRef)
            ]
        end
    }.

setup() ->
    skyraid_db_ets:init(), 
    {ok, User} = skyraid_user_repo:get_user(<<"Adam">>),
    {ok, Pid} = skyraid_user_session:start_link(User),
    Pid.

teardown(Pid) ->
    skyraid_db_ets:close(), 
    ok = skyraid_user_session:stop(Pid).

get_authentication_tc(Pid) ->
    {ok, Auth} = skyraid_user_session:get_authentication(Pid, "0"),
    ?_assert(is_record(Auth, skr_auth_basic)).

get_account_tc(Pid) ->
    AccountID = "0",
    {ok, #skr_account{id=ID}} = skyraid_user_session:get_account(Pid, AccountID),
    ?_assertEqual(AccountID, ID).

add_account_tc(Pid) ->
    ID = make_ref(),
    Account = #skr_account{id=ID, user_id="0"},
    {ok, #skr_session_info{accounts=[NewAccount|_Rest]}} = skyraid_user_session:add_account(Pid, Account),
    ?_assertEqual(Account, NewAccount).

-endif.