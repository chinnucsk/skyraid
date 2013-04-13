%% Copyright
%%
%% @doc Handles all request for a single authenticated user
%%
-module(skyraid_user_session).
-include("skyraid.hrl").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, info/1, add_account/2, get_authentication/2]).

start_link(User) ->
	gen_server:start_link(?MODULE, [User], []).

info(SessionRef) when is_pid(SessionRef) ->
	gen_server:call(SessionRef, get_info).

add_account(SessionRef, #skr_account{}=A) ->
    gen_server:call(SessionRef, {add_account, A}).

get_authentication(SessionRef, Storage) ->
    gen_server:call(SessionRef, {get_authentication, Storage}).

%% ====================================================================
%% State 
%% ====================================================================
-record(state, 
{
    timestamp = erlang:now(),
    user
}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([User]) ->
    {ok, #state{user=User}}.

handle_call(get_info, _From, S) ->
    Info = create_session_info(S),
	{reply, {ok, Info}, S};

handle_call({add_account, NewAccount}, _From, #state{user=#skr_user{accounts=CurrentAccounts}}=S) ->
    %% TODO add check for already existing or?
    NewState = S#state{user=#skr_user{accounts=CurrentAccounts ++ [NewAccount]}},
    Info = create_session_info(NewState),
    {reply, {ok, Info}, NewState};

handle_call({get_authentication, Storage}, _From, #state{user=#skr_user{accounts=Accounts}}=S) ->
    Replay = case [Auth || #skr_account{storage_id=AS, authentication=Auth} <- Accounts, Storage == AS] of
                [Auth] -> {ok, Auth};
                [] -> {error, not_found}
            end,
    {reply, Replay, S};

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

create_session_info(#state{timestamp=TimeStamp, user=User}) ->
    #skr_session_info{timestamp=TimeStamp, user=User}.