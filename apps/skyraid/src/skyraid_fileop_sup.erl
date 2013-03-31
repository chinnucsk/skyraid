%% Copyright
%%
%% @doc desc
%%

-module(skyraid_fileop_sup).
-behaviour(supervisor).
-export([init/1]).

-define(CHILD(Id, FileName, Opts), {Id, {skyraid_fileop2, start_link, [FileName, Opts]}, temporary, 5000, worker, [skyraid_fileop2]}).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start_fileop/2]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_fileop(FileName, Opts) ->
	supervisor:start_child(?MODULE, ?CHILD(make_ref(), FileName, Opts)).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%% ====================================================================
%% Internal functions
%% ====================================================================