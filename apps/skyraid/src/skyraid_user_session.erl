%% Copyright
%%
%% @doc Handles all request for a single authenticated user
%%
-module(skyraid_user_session).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, info/1]).

start_link(User) ->
	gen_server:start_link(?MODULE, [User], []).

info(SessionRef) when is_pid(SessionRef) ->
	gen_server:call(SessionRef, get_user_info).

%% ====================================================================
%% State 
%% ====================================================================
-record(state, {user}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([User]) ->
    {ok, #state{user=User}}.

handle_call(get_user_info, _From, #state{user=User}=State) ->
	{reply, User, State};

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