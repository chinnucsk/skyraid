-module(skyraid_webmachine_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    skyraid_webmachine_sup:start_link().

stop(_State) ->
    ok.
