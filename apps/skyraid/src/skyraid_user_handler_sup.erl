%% Copyright
%%
%% @doc Supervisor for user handlers
%%

-module(skyraid_user_handler_sup).
-behaviour(supervisor).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    {ok,{{one_for_all,0,1}, []}}.
	

%% ====================================================================
%% Internal functions
%% ====================================================================		