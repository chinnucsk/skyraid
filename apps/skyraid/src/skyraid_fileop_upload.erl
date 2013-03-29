%% Copyright
%%
%% @doc Process for uploading a file.
%%
-module(skyraid_fileop_upload).
-behaviour(gen_fsm).
-export([init/1, state_name/2, state_name/3, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, content/3]).

start_link() ->
	gen_fsm:start_link(?MODULE, []).

content(FileRef, Content, ChunkID) ->
    gen_fsm:sync_send_event(FileRef, {content, Content, id, ChunkID}).

%% ====================================================================
%% State 
%% ====================================================================
-record(state, {}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

init([]) ->
    {ok, state_name, #state{}}.

state_name(_Event, StateData) ->
    {next_state, state_name, StateData}.

state_name(_Event, _From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event({content, _Content, id, _ChunkID}, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================