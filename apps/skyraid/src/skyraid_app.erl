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

    skyraid_context:init(),
    ok = skyraid_context:register_account_provider(local, skyraid_account_provider_local),
    ok = skyraid_context:register_file_provider(local, skyraid_file_provider_local),
    
    skyraid_sup:start_link().

stop(_State) ->
    ok.
