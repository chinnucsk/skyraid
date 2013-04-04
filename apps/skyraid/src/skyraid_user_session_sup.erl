%% Copyright
%%
%% @doc Supervisor for user sessions
%%

-module(skyraid_user_session_sup).
-behaviour(supervisor).

-include("skyraid.hrl").

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, User), {Id, {skyraid_user_session, start_link, [User]}, temporary, 5000, worker, [skyraid_user_session]}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start_session/1, stop_session/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_session(User = #skr_user{uid=UID}) ->
	supervisor:start_child(?MODULE, ?CHILD(UID, User)).

stop_session(SessionRef) ->
	{ok, S} = skyraid_user_session:info(SessionRef),
	supervisor:terminate_child(?MODULE, S#skr_session_info.user#skr_user.uid).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    {ok,{{one_for_one,5,10}, []}}.
	

%% ====================================================================
%% Internal functions
%% ====================================================================		