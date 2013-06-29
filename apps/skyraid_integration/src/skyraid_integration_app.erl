-module(skyraid_integration_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	skyraid_context:register_account_provider(dropbox, skyraid_integration_dropbox),
	skyraid_context:register_file_provider(dropbox, skyraid_integration_dropbox),

    skyraid_integration_sup:start_link().

stop(_State) ->
    ok.
