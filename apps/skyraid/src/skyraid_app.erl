-module(skyraid_app).

-include("skyraid.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	?INFO("Starting skyraid"),
    skyraid_sup:start_link().

stop(_State) ->
    ok.
